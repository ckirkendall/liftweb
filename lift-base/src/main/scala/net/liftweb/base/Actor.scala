  /*
 * Copyright 2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

package net.liftweb.base

trait SimpleActor[T] {
  def !(param: T): Unit
}

trait SimplestActor extends SimpleActor[Any]

trait TypedActor[T, R] extends SimpleActor[T] {
  def !?(param: T): R
  def !?(param: T, timeout: Long): Option[R]
  def !!(param: T): Future[R]
}

/**
 * Generic Actor interface. Can send and receive any type of message.
 */
trait GenericActor extends SimpleActor[Any] {
  /**
   * Asynchronous message send. Fire-and-forget.
   */
  def !(message: Any): Unit

  /**
   * Emulates a synchronous call. Waits indefinitely on a Future for the reply.
   */
  def !?[R](message: Any): R

  /**
   * Asynchronous message send. Send-and-receive eventually. Waits on a Future for the reply message. 
   * If recevied within the Actor default timeout interval then it returns Some(result) and if a timeout 
   * has occured None. 
   */
  def !![R](message: Any): Option[R]

  /**
   * Asynchronous message send. Send-and-receive eventually. Waits on a Future for the reply message. 
   * If recevied within timout interval that is specified then it returns Some(result) and if a timeout 
   * has occured None. 
   */
  def !![R](message: Any, timeout: Long): Option[R]

  /**
   * Starts the Actor.
   */
  def start: Unit

  /**
   * Stops the Actor.
   */
  def stop: Unit
}

trait Future[T]

