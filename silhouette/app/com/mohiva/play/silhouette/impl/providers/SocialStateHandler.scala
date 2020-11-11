package com.mohiva.play.silhouette.impl.providers

import com.mohiva.play.silhouette.api.crypto.Signer
import com.mohiva.play.silhouette.api.crypto.Signer.Signer
import com.mohiva.play.silhouette.impl.providers.SocialStateItemHandler.SocialStateItemHandler
import zio.{Has, Task, ZLayer}

object SocialStateHandler {
  type SocialStateHandler = Has[Service]
  trait Service {

    /**
     * The concrete instance of the state handler.
     */
    type Self <: SocialStateHandler

    /**
     * The item handlers configured for this handler
     */
    val handlers: Set[SocialStateItemHandler]

    /**
     * Creates a copy of the state provider with a new handler added.
     *
     * There exists two types of state handlers. The first type are global state handlers which can be configured
     * by the user with the help of a configuration mechanism or through dependency injection. And there a local
     * state handlers which are provided by the application itself. This method exists to handle the last type of
     * state handlers, because it allows to extend the list of user defined state handlers from inside the library.
     *
     * @param handler The handler to add.
     * @return A new state provider with a new handler added.
     */
    def withHandler(handler: SocialStateItemHandler): Self

    /**
     * Gets the social state for all handlers.
     *
     * @return The social state for all handlers.
     */
    def state: Task[SocialState]

    /**
     * Serializes the given state into a single state value which can be passed with the state param.
     *
     * @param state The social state to serialize.
     * @return The serialized state as string.
     */
    def serialize(state: SocialState): String

    /**
     * Unserializes the social state from the state param.
     *
     * @param state The state to unserialize.
     * @tparam B The type of the request body.
     * @return The social state on success, an error on failure.
     */
    def unserialize[B](state: String): Task[SocialState]

    /**
     * Publishes the state to the client.
     *
     * @param state The state to publish.
     * @tparam B The type of the request body.
     * @return The result to send to the client.
     */
    def publish[B](state: SocialState): Any = ???
  }

  val live: ZLayer[Signer.Service, Throwable, Has[Service]] = ZLayer.fromService {
    (signer: Signer.Service) =>
    new Service {
      /**
       * The concrete instance of the state handler.
       */
      override type Self = Service
      /**
       * The item handlers configured for this handler
       */
      override val handlers: Set[SocialStateItemHandler.Service] = _

      /**
       * Creates a copy of the state provider with a new handler added.
       *
       * There exists two types of state handlers. The first type are global state handlers which can be configured
       * by the user with the help of a configuration mechanism or through dependency injection. And there a local
       * state handlers which are provided by the application itself. This method exists to handle the last type of
       * state handlers, because it allows to extend the list of user defined state handlers from inside the library.
       *
       * @param handler The handler to add.
       * @return A new state provider with a new handler added.
       */
      override def withHandler(handler: SocialStateItemHandler.Service): Self = ???

      /**
       * Gets the social state for all handlers.
       *
       * @return The social state for all handlers.
       */
      override def state: Task[SocialState] = Task.collectAll(handlers.map(_.item)).map(items => SocialState(items.toSet))

      /**
       * Serializes the given state into a single state value which can be passed with the state param.
       *
       * @param state The social state to serialize.
       * @return The serialized state as string.
       */
      override def serialize(state: SocialState): String = {
        signer.sign(state.items.flatMap { i =>
          Task.collectAll(handlers.map(h => h.canHandle(i).flatMap(h.serialize))).map(_.mkString)
        }.mkString("."))
      }

      /**
       * Unserializes the social state from the state param.
       *
       * @param state The state to unserialize.
       * @tparam B The type of the request body.
       * @return The social state on success, an error on failure.
       */
      override def unserialize[B](state: String): Task[SocialState] = ???
    }
  }
}
