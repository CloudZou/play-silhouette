/**
 * Original work: SecureSocial (https://github.com/jaliss/securesocial)
 * Copyright 2013 Jorge Aliss (jaliss at gmail dot com) - twitter: @jaliss
 *
 * Derivative work: Silhouette (https://github.com/mohiva/play-silhouette)
 * Modifications Copyright 2015 Mohiva Organisation (license at mohiva dot com)
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
package com.mohiva.play.silhouette.api.services

import com.mohiva.play.silhouette.api.auth.Result
import com.mohiva.play.silhouette.api.{Authenticator, LoginInfo}
import zio.{Has, Task}

/**
 * A marker result which indicates that an operation on an authenticator was processed and
 * therefore it shouldn't updated automatically.
 *
 * Due the fact that the update method gets called on every subsequent request to update the
 * authenticator related data in the backing store and in the result, it isn't possible to
 * discard or renew the authenticator simultaneously. This is because the "update" method would
 * override the result created by the "renew" or "discard" method, because it will be executed
 * as last in the chain.
 *
 * As example:
 * If we discard the session in a Silhouette action then it will be removed from session. But
 * at the end the update method will embed the session again, because it gets called with the
 * result of the action.
 *
 * @param result The result to wrap.
 */
class AuthenticatorResult(result: Result)
  extends Result{
}

/**
 * The companion object.
 */
object AuthenticatorResult {

  /**
   * Instantiates a new authenticator result.
   *
   * @param result The result to wrap.
   * @return An authenticator result.
   */
  def apply(result: Result) = new AuthenticatorResult(result)
}

object AuthenticatorService {
  type AuthenticatorService[T <: Authenticator] = Has[AuthenticatorService[T]]

  /**
   * The error messages.
   */
  val CreateError = "[Silhouette][%s] Could not create authenticator for login info: %s"
  val RetrieveError = "[Silhouette][%s] Could not retrieve authenticator"
  val InitError = "[Silhouette][%s] Could not init authenticator: %s"
  val UpdateError = "[Silhouette][%s] Could not update authenticator: %s"
  val RenewError = "[Silhouette][%s] Could not renew authenticator: %s"
  val DiscardError = "[Silhouette][%s] Could not discard authenticator: %s"
}

/**
 * Handles authenticators for the Silhouette module.
 *
 * @tparam T The type of the authenticator this service is responsible for.
 */
trait AuthenticatorService[T <: Authenticator] {

  /**
   * Creates a new authenticator for the specified login info.
   *
   * @param loginInfo The login info for which the authenticator should be created.
   * @return An authenticator.
   */
  def create(loginInfo: LoginInfo): Task[T]

  /**
   * Retrieves the authenticator from request.
   *
   * @tparam B The type of the request body.
   * @return Some authenticator or None if no authenticator could be found in request.
   */
  def retrieve[B](token: String): Task[T]

  /**
   * Initializes an authenticator and instead of embedding into the the request or result, it returns
   * the serialized value.
   *
   * @param authenticator The authenticator instance.
   * @return The serialized authenticator value.
   */
  def init(authenticator: T): Task[T#Value]

  /**
   * Embeds authenticator specific artifacts into the response.
   *
   * @param value The authenticator value to embed.
   * @param result The result to manipulate.
   * @return The manipulated result.
   */
  def embed(value: T#Value, result: Result): Task[AuthenticatorResult]

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
  def touch(authenticator: T): Either[T, T]

  /**
   * Updates a touched authenticator.
   *
   * If the authenticator was updated, then the updated artifacts should be embedded into the response.
   * This method gets called on every subsequent request if an identity accesses a Silhouette action,
   * expect the authenticator was not touched.
   *
   * @param authenticator The authenticator to update.
   * @param result The result to manipulate.
   * @return The original or a manipulated result.
   */
  def update(authenticator: T, result: Result): Task[AuthenticatorResult]

  /**
   * Renews the expiration of an authenticator without embedding it into the result.
   *
   * Based on the implementation, the renew method should revoke the given authenticator first, before
   * creating a new one. If the authenticator was updated, then the updated artifacts should be returned.
   *
   * @param authenticator The authenticator to renew.
   * @return The serialized expression of the authenticator.
   */
  def renew(authenticator: T): Task[T#Value]

  /**
   * Renews the expiration of an authenticator.
   *
   * Based on the implementation, the renew method should revoke the given authenticator first, before
   * creating a new one. If the authenticator was updated, then the updated artifacts should be embedded
   * into the response.
   *
   * @param authenticator The authenticator to renew.
   * @param result The result to manipulate.
   * @return The original or a manipulated result.
   */
  def renew(authenticator: T, result: Result): Task[AuthenticatorResult]

  /**
   * Manipulates the response and removes authenticator specific artifacts before sending it to the client.
   *
   * @param authenticator The authenticator instance.
   * @param result The result to manipulate.
   * @return The manipulated result.
   */
  def discard(authenticator: T, result: Result): Task[AuthenticatorResult]
}

