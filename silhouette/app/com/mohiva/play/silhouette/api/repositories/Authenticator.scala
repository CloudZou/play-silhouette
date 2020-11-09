package com.mohiva.play.silhouette.api.repositories

import com.mohiva.play.silhouette.api.StorableAuthenticator
import zio.{Has, Task}

object Authenticator {
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

}
