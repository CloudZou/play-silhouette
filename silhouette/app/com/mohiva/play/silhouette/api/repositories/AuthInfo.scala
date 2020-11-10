package com.mohiva.play.silhouette.api.repositories

import com.mohiva.play.silhouette.api.{AuthInfo, LoginInfo}
import zio.{Has, Layer, Task, ZLayer}

import scala.collection.mutable
import scala.reflect.ClassTag

object AuthInfo {
  type AuthInfoRepository[T <: AuthInfo] =  Has[Service[T]]

  trait Service[T <: AuthInfo] {
    /**
     * Finds the auth info which is linked with the specified login info.
     *
     * @param loginInfo The linked login info.
     * @return The found auth info or None if no auth info could be found for the given login info.
     */
    def find(loginInfo: LoginInfo): Task[Option[T]]

    /**
     * Adds new auth info for the given login info.
     *
     * @param loginInfo The login info for which the auth info should be saved.
     * @param authInfo The auth info to save.
     * @return The saved auth info.
     */
    def add(loginInfo: LoginInfo, authInfo: T): Task[T]

    /**
     * Updates the auth info for the given login info.
     *
     * @param loginInfo The login info for which the auth info should be updated.
     * @param authInfo The auth info to update.
     * @return The updated auth info.
     */
    def update(loginInfo: LoginInfo, authInfo: T): Task[T]

    /**
     * Saves the auth info for the given login info.
     *
     * This method either adds the auth info if it doesn't exists or it updates the auth info
     * if it already exists.
     *
     * @param loginInfo The login info for which the auth info should be saved.
     * @param authInfo The auth info to save.
     * @return The updated auth info.
     */
    def save(loginInfo: LoginInfo, authInfo: T): Task[T]

    /**
     * Removes the auth info for the given login info.
     *
     * @param loginInfo The login info for which the auth info should be removed.
     * @return A future to wait for the process to be completed.
     */
    def remove(loginInfo: LoginInfo): Task[Unit]
  }

  class InMemoryAuthInfoRepository[T <: AuthInfo] extends Service[T] {
    var data: mutable.HashMap[LoginInfo, T] = mutable.HashMap()
    /**
     * Finds the auth info which is linked with the specified login info.
     *
     * @param loginInfo The linked login info.
     * @return The found auth info or None if no auth info could be found for the given login info.
     */
    override def find(loginInfo: LoginInfo): Task[Option[T]] = Task.effect(data.get(loginInfo))

    /**
     * Adds new auth info for the given login info.
     *
     * @param loginInfo The login info for which the auth info should be saved.
     * @param authInfo  The auth info to save.
     * @return The saved auth info.
     */
    override def add(loginInfo: LoginInfo, authInfo: T): Task[T] = Task.effect {
      data += (loginInfo -> authInfo)
      authInfo
    }

    /**
     * Updates the auth info for the given login info.
     *
     * @param loginInfo The login info for which the auth info should be updated.
     * @param authInfo  The auth info to update.
     * @return The updated auth info.
     */
    override def update(loginInfo: LoginInfo, authInfo: T): Task[T] = add(loginInfo, authInfo)

    /**
     * Saves the auth info for the given login info.
     *
     * This method either adds the auth info if it doesn't exists or it updates the auth info
     * if it already exists.
     *
     * @param loginInfo The login info for which the auth info should be saved.
     * @param authInfo  The auth info to save.
     * @return The updated auth info.
     */
    override def save(loginInfo: LoginInfo, authInfo: T): Task[T] = add(loginInfo, authInfo)

    /**
     * Removes the auth info for the given login info.
     *
     * @param loginInfo The login info for which the auth info should be removed.
     * @return A future to wait for the process to be completed.
     */
    override def remove(loginInfo: LoginInfo): Task[Unit] = Task.effect {
      data -= loginInfo
      ()
    }
  }

  val live: Layer[Nothing, Has[Service[_]]] = ZLayer.succeed {
    new InMemoryAuthInfoRepository[_]
  }
}
