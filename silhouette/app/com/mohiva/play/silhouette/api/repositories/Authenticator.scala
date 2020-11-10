package com.mohiva.play.silhouette.api.repositories

import com.mohiva.play.silhouette.api.StorableAuthenticator
import com.mohiva.play.silhouette.api.util.Cache.CacheService
import com.mohiva.play.silhouette.api.util.Cache
import zio.{Has, Task, ZLayer}

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

object AuthenticatorRepository {
  type AuthenticatorRepository[T <: StorableAuthenticator] = Has[Service[T]]

  trait Service[T <: StorableAuthenticator] {
    /**
     * Finds the authenticator for the given ID.
     *
     * @param id The authenticator ID.
     * @return The found authenticator or None if no authenticator could be found for the given ID.
     */
    def find(id: String): Task[Option[T]]

    /**
     * Adds a new authenticator.
     *
     * @param authenticator The authenticator to add.
     * @return The added authenticator.
     */
    def add(authenticator: T): Task[T]

    /**
     * Updates an already existing authenticator.
     *
     * @param authenticator The authenticator to update.
     * @return The updated authenticator.
     */
    def update(authenticator: T): Task[T]

    /**
     * Removes the authenticator for the given ID.
     *
     * @param id The authenticator ID.
     * @return An empty future.
     */
    def remove(id: String): Task[Unit]
  }


  val live: ZLayer[CacheService, Throwable, Has[Service[_]]] = ZLayer.fromService { (cacheService: Cache.Service) =>
    class CacheAuthenticatorRepository[T <: StorableAuthenticator: ClassTag] extends Service[T] {
      /**
       * Finds the authenticator for the given ID.
       *
       * @param id The authenticator ID.
       * @return The found authenticator or None if no authenticator could be found for the given ID.
       */
      override def find(id: String): Task[Option[T]] = cacheService.find(id)

      /**
       * Adds a new authenticator.
       *
       * @param authenticator The authenticator to add.
       * @return The added authenticator.
       */
      override def add(authenticator: T): Task[T] = cacheService.save(authenticator.id, authenticator, Duration.Inf)

      /**
       * Updates an already existing authenticator.
       *
       * @param authenticator The authenticator to update.
       * @return The updated authenticator.
       */
      override def update(authenticator: T): Task[T] = cacheService.save(authenticator.id, authenticator, Duration.Inf)

      /**
       * Removes the authenticator for the given ID.
       *
       * @param id The authenticator ID.
       * @return An empty future.
       */
      override def remove(id: String): Task[Unit] = cacheService.remove(id)
    }
    new CacheAuthenticatorRepository[_]()
  }
}
