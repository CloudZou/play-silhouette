package com.mohiva.play.silhouette.impl.providers

import com.mohiva.play.silhouette.api.crypto.Signer
import com.mohiva.play.silhouette.api.crypto.Signer.Signer
import com.mohiva.play.silhouette.api.util.Generator.IDGenerator
import com.mohiva.play.silhouette.api.util.Generator.IDGenerator.IDGenerator
import com.mohiva.play.silhouette.impl.providers.SocialStateItem.ItemStructure
import com.mohiva.play.silhouette.impl.providers.state.CsrfStateItem
import zio.{Has, Task, ZLayer}

object SocialStateItemHandler {
  type SocialStateItemHandler = Has[Service]
  trait Service {
    /**
     * The item the handler can handle.
     */
    type Item <: SocialStateItem

    /**
     * Gets the state item the handler can handle.
     *
     * @return The state params the handler can handle.
     */
    def item: Task[Item]

    /**
     * Indicates if a handler can handle the given [[SocialStateItem]].
     *
     * This method should check if the [[serialize]] method of this handler can serialize the given
     * unserialized state item.
     *
     * @param item The item to check for.
     * @return `Some[Item]` casted state item if the handler can handle the given state item, `None` otherwise.
     */
    def canHandle(item: SocialStateItem): Task[Item]

    /**
     * Indicates if a handler can handle the given unserialized state item.
     *
     * This method should check if the [[unserialize]] method of this handler can unserialize the given
     * serialized state item.
     *
     * @param item    The item to check for.
     * @tparam B The type of the request body.
     * @return True if the handler can handle the given state item, false otherwise.
     */
    def canHandle[B](item: ItemStructure): Task[Boolean]

    /**
     * Returns a serialized value of the state item.
     *
     * @param item The state item to serialize.
     * @return The serialized state item.
     */
    def serialize(item: Item): Task[ItemStructure]

    /**
     * Unserializes the state item.
     *
     * @param item    The state item to unserialize.
     * @tparam B The type of the request body.
     * @return The unserialized state item.
     */
    def unserialize[B](item: ItemStructure): Task[Item]

    def publish[B](item: Item): Any = ???
  }


  /**
   * The ID of the handler.
   */
  val ID = "csrf-state"

  /**
   * The error messages.
   */
  val ClientStateDoesNotExists = "[Silhouette][CsrfStateItemHandler] State cookie doesn't exists for name: %s"

  val csrfLive: ZLayer[IDGenerator with Signer, Throwable, Has[Service]] = ZLayer.fromServices {
    (idGenerator: IDGenerator.Service, signer: Signer.Service) => new Service {
      /**
       * The item the handler can handle.
       */
      override type Item = CsrfStateItem

      /**
       * Gets the state item the handler can handle.
       *
       * @return The state params the handler can handle.
       */
      override def item: Task[Item] = idGenerator.generate.map(CsrfStateItem.apply)

      /**
       * Indicates if a handler can handle the given [[SocialStateItem]].
       *
       * This method should check if the [[serialize]] method of this handler can serialize the given
       * unserialized state item.
       *
       * @param item The item to check for.
       * @return `Some[Item]` casted state item if the handler can handle the given state item, `None` otherwise.
       */
      override def canHandle(item: SocialStateItem): Task[Item] = item match {
        case i: Item => Task.succeed(i)
        case _       => Task.fail(new RuntimeException("not exist"))
      }

      /**
       * Indicates if a handler can handle the given unserialized state item.
       *
       * This method should check if the [[unserialize]] method of this handler can unserialize the given
       * serialized state item.
       *
       * @param item The item to check for.
       * @tparam B The type of the request body.
       * @return True if the handler can handle the given state item, false otherwise.
       */
      override def canHandle[B](item: ItemStructure): Task[Boolean] =
        for {
          l <- Task.effect(item.id == ID)
          r <- signer.extract("").map(CsrfStateItem(_)).map(_ == item.data.as[Item])
        } yield l && r

      /**
       * Returns a serialized value of the state item.
       *
       * @param item The state item to serialize.
       * @return The serialized state item.
       */
      override def serialize(item: Item): Task[ItemStructure] = Task.effect(ItemStructure(ID, item.asJson))

      /**
       * Unserializes the state item.
       *
       * @param item The state item to unserialize.
       * @tparam B The type of the request body.
       * @return The unserialized state item.
       */
      override def unserialize[B](item: ItemStructure): Task[Item] = Task.fromEither(item.data.as[Item])
    }
  }
}
