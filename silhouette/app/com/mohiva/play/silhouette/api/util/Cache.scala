package com.mohiva.play.silhouette.api.util

import zio.{Has, Layer, Task, ZLayer}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

object Cache {
  type CacheService = Has[Service]

  trait Service {
    /**
     * Finds a value in the cache.
     *
     * @param key The key of the item to found.
     * @tparam T The type of the object to return.
     * @return The found value or None if no value could be found.
     */
    def find[T: ClassTag](key: String): Task[Option[T]]

    /**
     * Save a value in cache.
     *
     * @param key        The item key under which the value should be saved.
     * @param value      The value to save.
     * @param expiration Expiration time in seconds (0 second means eternity).
     * @return The value saved in cache.
     */
    def save[T](key: String, value: T, expiration: Duration = Duration.Inf): Task[T]

    /**
     * Remove a value from the cache.
     *
     * @param key Item key.
     * @return An empty future to wait for removal.
     */
    def remove(key: String): Task[Unit]
  }

  val live: Layer[Nothing, Has[Service]] = ZLayer.succeed {
    new Service {
      var data: mutable.HashMap[String, Any] = mutable.HashMap()

      /**
       * Finds a value in the cache.
       *
       * @param key The key of the item to found.
       * @tparam T The type of the object to return.
       * @return The found value or None if no value could be found.
       */
      override def find[T: ClassTag](key: String): Task[Option[T]] = Task.effect(data.get(key).map(_.asInstanceOf[T]))

      /**
       * Save a value in cache.
       *
       * @param key        The item key under which the value should be saved.
       * @param value      The value to save.
       * @param expiration Expiration time in seconds (0 second means eternity).
       * @return The value saved in cache.
       */
      override def save[T](key: String, value: T, expiration: Duration): Task[T] = Task.effect {
        data += (key -> value)
        value
      }

      /**
       * Remove a value from the cache.
       *
       * @param key Item key.
       * @return An empty future to wait for removal.
       */
      override def remove(key: String): Task[Unit] = Task.effect {
        data -= key
        ()
      }
    }
  }
}
