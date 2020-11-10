/**
 * Copyright 2015 Mohiva Organisation (license at mohiva dot com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mohiva.play.silhouette.impl.authenticators

import com.atlassian.jwt.SigningAlgorithm
import com.atlassian.jwt.core.writer.{JsonSmartJwtJsonBuilder, NimbusJwtWriterFactory}
import com.mohiva.play.silhouette.api.Authenticator.Implicits._
import com.mohiva.play.silhouette.api.crypto.{AuthenticatorEncoder, CryptoContext}
import com.mohiva.play.silhouette.api.exceptions._
import com.mohiva.play.silhouette.api.services.AuthenticatorService._
import com.mohiva.play.silhouette.api.services.{AuthenticatorResult, AuthenticatorService}
import com.mohiva.play.silhouette.api.util._
import com.mohiva.play.silhouette.api.{ExpirableAuthenticator, Logger, LoginInfo, StorableAuthenticator, auth}
import com.mohiva.play.silhouette.impl.authenticators.JWTAuthenticator._
import com.mohiva.play.silhouette.impl.authenticators.JWTAuthenticatorService.{ReservedClaims, _}
import com.nimbusds.jose.JWSObject
import com.nimbusds.jose.crypto.MACVerifier
import com.nimbusds.jwt.JWTClaimsSet
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.mvc.{RequestHeader, Result}
import com.mohiva.play.silhouette.ScalaCompat.JavaConverters._
import com.mohiva.play.silhouette.api.crypto.AuthenticatorEncoder.AuthenticatorEncoder
import com.mohiva.play.silhouette.api.repositories.Authenticator.AuthenticatorRepository
import com.mohiva.play.silhouette.api.repositories.AuthenticatorRepository.AuthenticatorRepository
import com.mohiva.play.silhouette.api.repositories.RepoContext
import com.mohiva.play.silhouette.api.util.Generator.IDGenerator.IDGenerator
import com.mohiva.play.silhouette.settings.ConfigurationProvider.ConfigurationProvider
import zio.{Task, ZIO, ZLayer}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * An authenticator that uses a header based approach with the help of a JWT. It works by
 * using a JWT to transport the authenticator data inside a user defined header. It can
 * be stateless with the disadvantages that the JWT can't be invalidated.
 *
 * The authenticator can use sliding window expiration. This means that the authenticator times
 * out after a certain time if it wasn't used. This can be controlled with the [[idleTimeout]]
 * property. If this feature is activated then a new token will be generated on every update.
 * Make sure your application can handle this case.
 *
 * @see http://self-issued.info/docs/draft-ietf-oauth-json-web-token.html#Claims
 * @see https://developer.atlassian.com/static/connect/docs/concepts/understanding-jwt.html
 *
 * @param id                 The authenticator ID.
 * @param loginInfo          The linked login info for an identity.
 * @param lastUsedDateTime   The last used date/time.
 * @param expirationDateTime The expiration date/time.
 * @param idleTimeout        The duration an authenticator can be idle before it timed out.
 * @param customClaims       Custom claims to embed into the token.
 */
case class JWTAuthenticator(
  id: String,
  loginInfo: LoginInfo,
  lastUsedDateTime: DateTime,
  expirationDateTime: DateTime,
  idleTimeout: Option[FiniteDuration],
  customClaims: Option[JsObject] = None)
  extends StorableAuthenticator with ExpirableAuthenticator {

  /**
   * The Type of the generated value an authenticator will be serialized to.
   */
  override type Value = String
}

/**
 * The companion object.
 */
object JWTAuthenticator {

  /**
   * Serializes the authenticator.
   *
   * @param authenticator        The authenticator to serialize.
   * @param settings             The authenticator settings.
   * @return The serialized authenticator.
   */
  def serialize(
    authenticator: JWTAuthenticator,
    settings: JWTAuthenticatorSettings): ZIO[AuthenticatorEncoder, Throwable, String] = {

    val internalSerialize = (encodedSubject: String) => Task.effect {
      val jwtBuilder = new JsonSmartJwtJsonBuilder()
        .jwtId(authenticator.id)
        .issuer(settings.issuerClaim)
        .subject(encodedSubject)
        .issuedAt(authenticator.lastUsedDateTime.getMillis / 1000)
        .expirationTime(authenticator.expirationDateTime.getMillis / 1000)

      authenticator.customClaims.foreach { data =>
        serializeCustomClaims(data).asScala.foreach {
          case (key, value) =>
            if (ReservedClaims.contains(key)) {
              throw new AuthenticatorException(OverrideReservedClaim.format(ID, key, ReservedClaims.mkString(", ")))
            }
            jwtBuilder.claim(key, value)
        }
      }

      new NimbusJwtWriterFactory()
        .macSigningWriter(SigningAlgorithm.HS256, settings.sharedSecret)
        .jsonToJwt(jwtBuilder.build())
    }

    for {
      subject <- zio.blocking.effectBlocking(Json.toJson(authenticator.loginInfo).toString())
      encodedSubject <- ZIO.accessM[AuthenticatorEncoder](_.get.encode(subject))
      jwtToken <- internalSerialize(encodedSubject)
    } yield jwtToken
  }

  /**
   * Unserializes the authenticator.
   *
   * @param str                  The string representation of the authenticator.
   * @param settings             The authenticator settings.
   * @return An authenticator on success, otherwise a failure.
   */
  def unserialize(
    str: String,
    settings: JWTAuthenticatorSettings): ZIO[AuthenticatorEncoder, Throwable, JWTAuthenticator] = {

    val internalUnSerialize = (subject: String, c: JWTClaimsSet) => Task.fromTry {
      buildLoginInfo(subject).map { loginInfo =>
        val filteredClaims = c.getClaims.asScala.filterNot { case (k, v) => ReservedClaims.contains(k) || v == null }
        val customClaims = unserializeCustomClaims(filteredClaims.asJava)
        JWTAuthenticator(
          id = c.getJWTID,
          loginInfo = loginInfo,
          lastUsedDateTime = new DateTime(c.getIssueTime),
          expirationDateTime = new DateTime(c.getExpirationTime),
          idleTimeout = settings.authenticatorIdleTimeout,
          customClaims = if (customClaims.keys.isEmpty) None else Some(customClaims)
        )
      }
    } mapError {
      case e => throw new AuthenticatorException(InvalidJWTToken.format(ID, str), e)
    }

    val parser = Task.effect {
      val verifier = new MACVerifier(settings.sharedSecret)
      val jwsObject = JWSObject.parse(str)
      if (!jwsObject.verify(verifier)) {
        throw new IllegalArgumentException("Fraudulent JWT token: " + str)
      }

      JWTClaimsSet.parse(jwsObject.getPayload.toJSONObject)
    }

    for {
      jwtClaimSet <- parser
      subject <- ZIO.accessM[AuthenticatorEncoder](_.get.decode(jwtClaimSet.getSubject))
      authenticator <- internalUnSerialize(subject, jwtClaimSet)
    } yield authenticator
  }

  /**
   * Serializes recursively the custom claims.
   *
   * @param claims The custom claims to serialize.
   * @return A map containing custom claims.
   */
  private def serializeCustomClaims(claims: JsObject): java.util.Map[String, Any] = {
    def toJava(value: JsValue): Any = value match {
      case v: JsString  => v.value
      case v: JsNumber  => v.value
      case v: JsBoolean => v.value
      case v: JsObject  => serializeCustomClaims(v)
      case v: JsArray   => v.value.map(toJava).asJava
      case v            => throw new AuthenticatorException(UnexpectedJsonValue.format(ID, v))
    }

    claims.fieldSet.map { case (name, value) => name -> toJava(value) }.toMap.asJava
  }

  /**
   * Unserializes recursively the custom claims.
   *
   * @param claims The custom claims to deserialize.
   * @return A Json object representing the custom claims.
   */
  private def unserializeCustomClaims(claims: java.util.Map[String, AnyRef]): JsObject = {
    def toJson(value: Any): JsValue = value match {
      case v: java.lang.String    => JsString(v)
      case v: java.lang.Number    => JsNumber(BigDecimal(v.toString))
      case v: java.lang.Boolean   => JsBoolean(v)
      case v: java.util.Map[_, _] => unserializeCustomClaims(v.asInstanceOf[java.util.Map[String, AnyRef]])
      case v: java.util.List[_]   => JsArray(v.asScala.map(toJson))
      case v                      => throw new AuthenticatorException(UnexpectedJsonValue.format(ID, v))
    }

    JsObject(claims.asScala.map { case (name, value) => name -> toJson(value) }.toSeq)
  }

  /**
   * Builds the login info from Json.
   *
   * @param str The string representation of the login info.
   * @return The login info on success, otherwise a failure.
   */
  private def buildLoginInfo(str: String): Try[LoginInfo] = {
    Try(Json.parse(str)) match {
      case Success(json) =>
        // We needn't check here if the given Json is a valid LoginInfo object, because the
        // token will be signed and therefore the login info can't be manipulated. So if we
        // serialize an authenticator into a JWT, then this JWT is always the same authenticator
        // after deserialization
        Success(json.as[LoginInfo])
      case Failure(error) =>
        // This error can occur if an authenticator was serialized with the setting encryptSubject=true
        // and deserialized with the setting encryptSubject=false
        Failure(new AuthenticatorException(JsonParseError.format(ID, str), error))
    }
  }
}

object JWTAuthenticatorService {
  val live: ZLayer[RepoContext, Throwable, AuthenticatorService[_]] = ZLayer.fromServices {
    (authenticatorEncoder: AuthenticatorEncoder, configurationProvider: ConfigurationProvider, idGenerator: IDGenerator, authenticatorRepository: AuthenticatorRepository[JWTAuthenticator]) =>
      new AuthenticatorService[JWTAuthenticator] {
        private val settings = configurationProvider.get.underlying.asInstanceOf[JWTAuthenticatorSettings]
        /**
         * Creates a new authenticator for the specified login info.
         *
         * @param loginInfo The login info for which the authenticator should be created.
         * @return An authenticator.
         */
        override def create(loginInfo: LoginInfo): Task[JWTAuthenticator] = {
          val jwtAuthenticator = (id: String) => Task.effect {
            val now = DateTime.now
            JWTAuthenticator(
              id = id,
              loginInfo = loginInfo,
              lastUsedDateTime = now,
              expirationDateTime = now + settings.authenticatorExpiry,
              idleTimeout = settings.authenticatorIdleTimeout
            )
          } mapError{
            e => throw new AuthenticatorCreationException(CreateError.format(ID, loginInfo), e)
          }
          for {
            id <- idGenerator.get.generate
            authenticator <- jwtAuthenticator(id)
          } yield authenticator
        }

        /**
         * Retrieves the authenticator from request.
         *
         * @tparam B The type of the request body.
         * @return Some authenticator or None if no authenticator could be found in request.
         */
        override def retrieve[B](token: String): Task[JWTAuthenticator] = unserialize(token, settings).provide(authenticatorEncoder)

        /**
         * Initializes an authenticator and instead of embedding into the the request or result, it returns
         * the serialized value.
         *
         * @param authenticator The authenticator instance.
         * @return The serialized authenticator value.
         */
        override def init(authenticator: JWTAuthenticator): Task[String] = {
          authenticatorRepository.get.add(authenticator).flatMap(_ => serialize(authenticator, settings)).provide(authenticatorEncoder)
        }

        /**
         * Embeds authenticator specific artifacts into the response.
         *
         * @param value  The authenticator value to embed.
         * @param result The result to manipulate.
         * @return The manipulated result.
         */
        override def embed(value: String, result: auth.Result): Task[AuthenticatorResult] = ???

        /**
         * Touches an authenticator.
         *
         * An authenticator can use sliding window expiration. This means that the authenticator times
         * out after a certain time if it wasn't used. So to mark an authenticator as used it will be
         * touched on every request to a Silhouette action. If an authenticator should not be touched
         * because of the fact that sliding window expiration is disabled, then it should be returned
         * on the right, otherwise it should be returned on the left. An untouched authenticator needn't
         * be updated later by the [[update]] method.
         *
         * @param authenticator The authenticator to touch.
         * @return The touched authenticator on the left or the untouched authenticator on the right.
         */
        override def touch(authenticator: JWTAuthenticator): Either[JWTAuthenticator, JWTAuthenticator] = ???

        /**
         * Updates a touched authenticator.
         *
         * If the authenticator was updated, then the updated artifacts should be embedded into the response.
         * This method gets called on every subsequent request if an identity accesses a Silhouette action,
         * expect the authenticator was not touched.
         *
         * @param authenticator The authenticator to update.
         * @param result        The result to manipulate.
         * @return The original or a manipulated result.
         */
        override def update(authenticator: JWTAuthenticator, result: auth.Result): Task[AuthenticatorResult] = ???

        /**
         * Renews the expiration of an authenticator without embedding it into the result.
         *
         * Based on the implementation, the renew method should revoke the given authenticator first, before
         * creating a new one. If the authenticator was updated, then the updated artifacts should be returned.
         *
         * @param authenticator The authenticator to renew.
         * @return The serialized expression of the authenticator.
         */
        override def renew(authenticator: JWTAuthenticator): Task[String] = ???

        /**
         * Renews the expiration of an authenticator.
         *
         * Based on the implementation, the renew method should revoke the given authenticator first, before
         * creating a new one. If the authenticator was updated, then the updated artifacts should be embedded
         * into the response.
         *
         * @param authenticator The authenticator to renew.
         * @param result        The result to manipulate.
         * @return The original or a manipulated result.
         */
        override def renew(authenticator: JWTAuthenticator, result: auth.Result): Task[AuthenticatorResult] = ???

        /**
         * Manipulates the response and removes authenticator specific artifacts before sending it to the client.
         *
         * @param authenticator The authenticator instance.
         * @param result        The result to manipulate.
         * @return The manipulated result.
         */
        override def discard(authenticator: JWTAuthenticator, result: auth.Result): Task[AuthenticatorResult] = ???
      }

  }


  /**
   * The ID of the authenticator.
   */
  val ID = "jwt-authenticator"

  /**
   * The error messages.
   */
  val InvalidJWTToken = "[Silhouette][%s] Error on parsing JWT token: %s"
  val JsonParseError = "[Silhouette][%s] Cannot parse Json: %s"
  val UnexpectedJsonValue = "[Silhouette][%s] Unexpected Json value: %s"
  val OverrideReservedClaim = "[Silhouette][%s] Try to overriding a reserved claim `%s`; list of reserved claims: %s"

  /**
   * The reserved claims used by the authenticator.
   */
  val ReservedClaims = Seq("jti", "iss", "sub", "iat", "exp")
}
class JWTAuthenticatorService(
  settings: JWTAuthenticatorSettings,
  authenticatorEncoder: AuthenticatorEncoder)
  extends AuthenticatorService[JWTAuthenticator]
  with Logger {

  /**
   * Creates a new authenticator for the specified login info.
   *
   * @param loginInfo The login info for which the authenticator should be created.
   * @param request The request header.
   * @return An authenticator.
   */
  override def create(loginInfo: LoginInfo)(implicit request: RequestHeader): Future[JWTAuthenticator] = {
    idGenerator.generate.map { id =>
      val now = clock.now
      JWTAuthenticator(
        id = id,
        loginInfo = loginInfo,
        lastUsedDateTime = now,
        expirationDateTime = now + settings.authenticatorExpiry,
        idleTimeout = settings.authenticatorIdleTimeout
      )
    }.recover {
      case e => throw new AuthenticatorCreationException(CreateError.format(ID, loginInfo), e)
    }
  }

  /**
   * Retrieves the authenticator from request.
   *
   * If a backing store is defined, then the authenticator will be validated against it.
   *
   * @param request The request to retrieve the authenticator from.
   * @tparam B The type of the request body.
   * @return Some authenticator or None if no authenticator could be found in request.
   */
  override def retrieve[B](implicit request: ExtractableRequest[B]): Future[Option[JWTAuthenticator]] = {
    Future.fromTry(Try(request.extractString(settings.fieldName, settings.requestParts))).flatMap {
      case Some(token) => unserialize(token, authenticatorEncoder, settings) match {
        case Success(authenticator) => repository.fold(Future.successful(Option(authenticator)))(_.find(authenticator.id))
        case Failure(e) =>
          logger.info(e.getMessage, e)
          Future.successful(None)
      }
      case None => Future.successful(None)
    }.recover {
      case e => throw new AuthenticatorRetrievalException(RetrieveError.format(ID), e)
    }
  }

  /**
   * Creates a new JWT for the given authenticator and return it. If a backing store is defined, then the
   * authenticator will be stored in it.
   *
   * @param authenticator The authenticator instance.
   * @param request       The request header.
   * @return The serialized authenticator value.
   */
  override def init(authenticator: JWTAuthenticator)(implicit request: RequestHeader): Future[String] = {
    repository.fold(Future.successful(authenticator))(_.add(authenticator)).map { a =>
      serialize(a, authenticatorEncoder, settings)
    }.recover {
      case e => throw new AuthenticatorInitializationException(InitError.format(ID, authenticator), e)
    }
  }

  /**
   * Adds a header with the token as value to the result.
   *
   * @param token  The token to embed.
   * @param result The result to manipulate.
   * @return The manipulated result.
   */
  override def embed(token: String, result: Result)(implicit request: RequestHeader): Future[AuthenticatorResult] = {
    Future.successful(AuthenticatorResult(result.withHeaders(settings.fieldName -> token)))
  }

  /**
   * Adds a header with the token as value to the request.
   *
   * @param token   The token to embed.
   * @param request The request header.
   * @return The manipulated request header.
   */
  override def embed(token: String, request: RequestHeader): RequestHeader = {
    val additional = Seq(settings.fieldName -> token)
    request.withHeaders(request.headers.replace(additional: _*))
  }

  /**
   * @inheritdoc
   *
   * @param authenticator The authenticator to touch.
   * @return The touched authenticator on the left or the untouched authenticator on the right.
   */
  override def touch(authenticator: JWTAuthenticator): Either[JWTAuthenticator, JWTAuthenticator] = {
    if (authenticator.idleTimeout.isDefined) {
      Left(authenticator.copy(lastUsedDateTime = clock.now))
    } else {
      Right(authenticator)
    }
  }

  /**
   * Updates the authenticator and embeds a new token in the result.
   *
   * To prevent the creation of a new token on every request, disable the idle timeout setting and this
   * method will not be executed.
   *
   * @param authenticator The authenticator to update.
   * @param result        The result to manipulate.
   * @param request       The request header.
   * @return The original or a manipulated result.
   */
  override def update(authenticator: JWTAuthenticator, result: Result)(
    implicit
    request: RequestHeader): Future[AuthenticatorResult] = {

    repository.fold(Future.successful(authenticator))(_.update(authenticator)).map { a =>
      AuthenticatorResult(result.withHeaders(settings.fieldName -> serialize(a, authenticatorEncoder, settings)))
    }.recover {
      case e => throw new AuthenticatorUpdateException(UpdateError.format(ID, authenticator), e)
    }
  }

  /**
   * Renews an authenticator.
   *
   * After that it isn't possible to use a JWT which was bound to this authenticator. This method
   * doesn't embed the the authenticator into the result. This must be done manually if needed
   * or use the other renew method otherwise.
   *
   * @param authenticator The authenticator to renew.
   * @param request       The request header.
   * @return The serialized expression of the authenticator.
   */
  override def renew(authenticator: JWTAuthenticator)(implicit request: RequestHeader): Future[String] = {
    repository.fold(Future.successful(()))(_.remove(authenticator.id)).flatMap { _ =>
      create(authenticator.loginInfo).map(_.copy(customClaims = authenticator.customClaims)).flatMap(init)
    }.recover {
      case e => throw new AuthenticatorRenewalException(RenewError.format(ID, authenticator), e)
    }
  }

  /**
   * Renews an authenticator and replaces the JWT header with a new one.
   *
   * If a backing store is defined, the old authenticator will be revoked. After that, it isn't
   * possible to use a JWT which was bound to this authenticator.
   *
   * @param authenticator The authenticator to update.
   * @param result        The result to manipulate.
   * @param request       The request header.
   * @return The original or a manipulated result.
   */
  override def renew(authenticator: JWTAuthenticator, result: Result)(
    implicit
    request: RequestHeader): Future[AuthenticatorResult] = {

    renew(authenticator).flatMap(v => embed(v, result)).recover {
      case e => throw new AuthenticatorRenewalException(RenewError.format(ID, authenticator), e)
    }
  }

  /**
   * Removes the authenticator from backing store.
   *
   * @param result  The result to manipulate.
   * @param request The request header.
   * @return The manipulated result.
   */
  override def discard(authenticator: JWTAuthenticator, result: Result)(
    implicit
    request: RequestHeader): Future[AuthenticatorResult] = {

    repository.fold(Future.successful(()))(_.remove(authenticator.id)).map { _ =>
      AuthenticatorResult(result)
    }.recover {
      case e => throw new AuthenticatorDiscardingException(DiscardError.format(ID, authenticator), e)
    }
  }
}

/**
 * The companion object of the authenticator service.
 */
object JWTAuthenticatorService {

  /**
   * The ID of the authenticator.
   */
  val ID = "jwt-authenticator"

  /**
   * The error messages.
   */
  val InvalidJWTToken = "[Silhouette][%s] Error on parsing JWT token: %s"
  val JsonParseError = "[Silhouette][%s] Cannot parse Json: %s"
  val UnexpectedJsonValue = "[Silhouette][%s] Unexpected Json value: %s"
  val OverrideReservedClaim = "[Silhouette][%s] Try to overriding a reserved claim `%s`; list of reserved claims: %s"

  /**
   * The reserved claims used by the authenticator.
   */
  val ReservedClaims = Seq("jti", "iss", "sub", "iat", "exp")
}

/**
 * The settings for the JWT authenticator.
 *
 * @param fieldName                The name of the field in which the token will be transferred in any part
 *                                 of the request.
 * @param requestParts             Some request parts from which a value can be extracted or None to extract
 *                                 values from any part of the request.
 * @param issuerClaim              The issuer claim identifies the principal that issued the JWT.
 * @param authenticatorIdleTimeout The duration an authenticator can be idle before it timed out.
 * @param authenticatorExpiry      The duration an authenticator expires after it was created.
 * @param sharedSecret             The shared secret to sign the JWT.
 */
case class JWTAuthenticatorSettings(
  fieldName: String = "X-Auth-Token",
  requestParts: Option[Seq[RequestPart.Value]] = Some(Seq(RequestPart.Headers)),
  issuerClaim: String = "play-silhouette",
  authenticatorIdleTimeout: Option[FiniteDuration] = None,
  authenticatorExpiry: FiniteDuration = 12 hours,
  sharedSecret: String)
