package com.mohiva.play.silhouette.api.auth

import com.mohiva.play.silhouette.api.{Env, Environment, LoginInfo, RequestProvider}
import zio.Task

trait ContextHandlerBuilder[E <: Env, +R[_]] {
  protected implicit class ExtractEither[T](r: Either[T, T]) {
    def extract: T = r.fold(identity, identity)
  }

  val environment: Environment[E]


  final def apply[T](block: R[Any] => Task[HandlerResult[T]]): Task[HandlerResult[T]] =
    invokeBlock(block)


  def invokeBlock[B, T](block: R[B] => Task[HandlerResult[T]]): Task[HandlerResult[T]]

  protected def handleBlock[T](authenticator: Either[E#A, E#A], block: E#A => Task[HandlerResult[T]]) = {
    authenticator match {
      case Left(a) => handleInitializedAuthenticator(a, block)
      case Right(a) => handleUninitializedAuthenticator(a, block)
    }
  }

  protected def handleAuthentication[B]: Task[(Option[Either[E#A, E#A]], Option[E#I])] = {
    environment.authenticatorService.retrieve.flatMap {
      case Some(a) if a.isValid => environment.identityService.get.retrieve(a.loginInfo).map(i => Some(Left(a)) -> i)
      case Some(a) if !a.isValid => Task.effect(Some(Left(a)) -> None)
      case None => handleRequestProviderAuthentication.flatMap {
        case Some(loginInfo) => environment.identityService.get.retrieve(loginInfo).flatMap { i =>
          environment.authenticatorService.create(loginInfo).map(a => Some(Right(a)) -> i)
        }
        case None => Task.effect(None -> None)
      }
    }
  }

  private def handleInitializedAuthenticator[T](authenticator: E#A, block: E#A => Task[HandlerResult[T]]) = {
    val authenticatorInstance = environment.authenticatorService.touch(authenticator)
    block(authenticatorInstance.fold(identity, identity)).flatMap {
      case hr @ HandlerResult(result, _) => Task.effect(hr)
      case hr @HandlerResult(pr, _) => authenticatorInstance match {
        case Left(a) => environment.authenticatorService.update(a, pr).map(pr => hr.copy(pr))
        case Right(a) => Task.effect(hr)
      }
    }
  }

  private def handleUninitializedAuthenticator[T](authenticator: E#A, block: E#A => Task[HandlerResult[T]]) = {
    block(authenticator).flatMap {
      case hr @ HandlerResult(result, _) => Task.effect(hr)
      case hr @HandlerResult(pr, _) =>
        environment.authenticatorService.init(authenticator).flatMap { value =>
          environment.authenticatorService.embed(value, pr)
      }.map(pr => hr.copy(pr))
    }
  }

  private def handleRequestProviderAuthentication[B]: Task[Option[LoginInfo]] = {
    def auth(providers: Seq[RequestProvider]): Task[Option[LoginInfo]] = {
      providers match {
        case Nil => Task.effect(None)
        case h :: t => h.authenticate.flatMap {
          case Some(i) => Task.effect(Some(i))
          case None => if (t.isEmpty) Task.effect(None) else auth(t)
        }
      }
    }
    auth(environment.requestProviders)
  }
}
