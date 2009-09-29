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
  def !?(timeout: Long, param: T): Option[R]
  def !!(param: T): Future[R]
}

trait Actor extends SimpleActor[Any] {
  def !(message: Any): Unit
  def !?[T](message: Any): T
  def !![R](message: Any): Option[R]
  def !![R](message: Any, timeout: Long): Option[R]
  def start: Unit
  def stop: Unit
}
