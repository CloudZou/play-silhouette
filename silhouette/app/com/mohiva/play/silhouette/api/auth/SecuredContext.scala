package com.mohiva.play.silhouette.api.auth

import com.mohiva.play.silhouette.api.{Authorization, Env, Environment}
import zio.Task

trait SecuredContext[E <: Env, +B] {
  /**
   * @return The identity implementation.
   */
  def identity: E#I

  /**
   * @return The authenticator implementation.
   */
  def authenticator: E#A
}

object SecuredContext {
  def apply[E <: Env, B](identity: E#I, authenticator: E#A): SecuredContext[E, B] = {
    new DefaultSecuredContext(identity, authenticator)
  }
}

class DefaultSecuredContext[E <: Env, B](
                                        val identity: E#I,
                                        val authenticator: E#A
                                        ) extends SecuredContext[E,B]


case class
SecuredContextHandlerBuilder[E <: Env](
                                                   environment: Environment[E],
                                                   authorization: Option[Authorization[E#I, E#A]])
extends ContextHandlerBuilder[E, ({ type R[B] = SecuredContext[E, B] })#R] {
  override def invokeBlock[B, T](block: SecuredContext[E, B] => Task[HandlerResult[T]]): Task[HandlerResult[T]] = {
    withAuthorization(handleAuthentication).flatMap {
      case (Some(authenticator), Some(identity), Some(authorized)) if authorized =>
        handleBlock(authenticator, a => block(SecuredContext(identity, a)))
      case (Some(authenticator), Some(identity), _) =>
        handleBlock(authenticator, b => block(SecuredContext(identity, b)))
    }
  }


  private def withAuthorization[B](result: Task[(Option[Either[E#A, E#A]], Option[E#I])]) = {
    result.flatMap {
      case (Some(a), Some(i)) =>
        authorization.map(_.isAuthorized(i, a.extract)).getOrElse(Task.effect(true)).map(b => (Some(a), Some(i), Some(b)))
      case (a, i) =>
        Task.effect((a, i, None))
    }
  }
}
