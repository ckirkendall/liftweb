package net.liftweb.mapper

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
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

import _root_.java.sql.{ResultSet, Types}
import _root_.java.lang.reflect.Method
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.java.util.Date
import _root_.scala.xml.{NodeSeq, Text, Unparsed}
import _root_.net.liftweb.http.{S, SHtml}
import _root_.net.liftweb.http.js._
import S._

class MappedLongForeignKey[T<:Mapper[T],O<:KeyedMapper[Long, O]](theOwner: T, foreign: => KeyedMetaMapper[Long, O])
extends MappedLong[T](theOwner) with MappedForeignKey[Long,T,O] with BaseForeignKey {
  def defined_? = /*i_get_! != defaultValue &&*/ i_is_! > 0L

  def can: Box[Long] = if (defined_?) Full(is) else Empty

  type KeyType = Long
  type KeyedForeignType = O
  type OwnerType = T

  override def jdbcFriendly(field : String) = if (defined_?) new _root_.java.lang.Long(i_is_!) else null
  override def jdbcFriendly = if (defined_?) new _root_.java.lang.Long(i_is_!) else null

  lazy val dbKeyToTable: KeyedMetaMapper[Long, O] = foreign
  def dbKeyToColumn = dbKeyToTable.primaryKeyField

  override def dbIndexed_? = true

  override def dbForeignKey_? = true


  def asSafeJs(obs: Box[KeyObfuscator]): JsExp =
  obs.map(o => JE.Str(o.obscure(dbKeyToTable, is))).openOr(JE.Num(is))

  /**
   * Called when Schemifier adds a foreign key.  Return a function that will be called when Schemifier
   * is done with the schemification.
   */
  def dbAddedForeignKey: Box[() => Unit] = Empty

  override def toString = if (defined_?) super.toString else "NULL"

  /*
   def :=(v : Box[O]) : Long = this.:=(v.map(_.primaryKeyField.is) openOr 0L)
   def :=(v : O) : Long = this.:=(v.primaryKeyField.is)
   */

  def apply(v: Box[O]): T = this(v.map(_.primaryKeyField.is) openOr 0L)
  def apply(v: O): T = this(v.primaryKeyField.is)

  def findFor(key: KeyType): List[OwnerType] = theOwner.getSingleton.findAll(By(this, key))

  def findFor(key: KeyedForeignType): List[OwnerType] = theOwner.getSingleton.findAll(By(this, key))

  def +(in: Long): Long = is + in

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longForeignKeyColumnType  + notNullAppender()

}

class MappedLongIndex[T<:Mapper[T]](theOwner: T) extends MappedLong[T](theOwner) with IndexedField[Long] {

  override def writePermission_? = false // not writable

  override def dbIndexed_? = true

  def defined_? = i_is_! != defaultValue
  override def dbPrimaryKey_? = true

  override def defaultValue = -1L

  override def dbIndexFieldIndicatesSaved_? = {i_is_! != defaultValue}

  def makeKeyJDBCFriendly(in: Long) = new _root_.java.lang.Long(in)

  def convertKey(in: String): Box[Long] = {
    if (in eq null) Empty
    else tryo(toLong(if (in.startsWith(name + "=")) in.substring((name + "=").length) else in))
  }

  override def dbDisplay_? = false

  def convertKey(in : Long): Box[Long] = {
    if (in < 0L) Empty
    else Full(in)
  }

  def convertKey(in : Int): Box[Long] = {
    if (in < 0) Empty
    else Full(in)
  }

  def convertKey(in : AnyRef): Box[Long] = {
    if ((in eq null) || (in eq None)) Empty
    else tryo(convertKey(in.toString)).flatMap(s => s)
  }

  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longIndexColumnType  + notNullAppender()

}

class MappedEnumList[T<:Mapper[T], ENUM <: Enumeration](val fieldOwner: T, val enum: ENUM) extends MappedField[Seq[ENUM#Value], T] {
  private var data: Seq[ENUM#Value] = defaultValue
  private var orgData: Seq[ENUM#Value] = defaultValue

  def defaultValue: Seq[ENUM#Value] = Nil
  def dbFieldClass = classOf[Seq[ENUM#Value]]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_! = data
  protected def i_was_! = orgData
  /**
   * Called after the field is saved to the database
   */
  override protected[mapper] def doneWithSave() {
    orgData = data
  }

  protected def real_i_set_!(value: Seq[ENUM#Value]): Seq[ENUM#Value] = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true

  def asJsExp: JsExp = JE.JsArray(is.map(v => JE.Num(v.id)) :_*)

  def real_convertToJDBCFriendly(value: Seq[ENUM#Value]): Object = new _root_.java.lang.Long(Helpers.toLong(value))

  private def toLong: Long = is.foldLeft(enum.Set64)((a,b) => a + b.asInstanceOf[enum.Value]).underlyingAsLong

  def fromLong(in: Long): Seq[ENUM#Value] = enum.Set64(in).toList

  def jdbcFriendly(field: String) = new _root_.java.lang.Long(toLong)
  override def jdbcFriendly = new _root_.java.lang.Long(toLong)



  override def setFromAny(in: Any): Seq[ENUM#Value] = {
    in match {
      case n: Long => this.set( fromLong(n))
      case n: Number => this.set(fromLong(n.longValue))
      case (n: Number) :: _ => this.set(fromLong(n.longValue))
      case Some(n: Number) => this.set(fromLong(n.longValue))
      case None => this.set(Nil)
      case (s: String) :: _ => this.set(fromLong(Helpers.toLong(s)))
      case vs: List[ENUM#Value] => this.set(vs)
      case null => this.set(Nil)
      case s: String => this.set(fromLong(Helpers.toLong(s)))
      case o => this.set(fromLong(Helpers.toLong(o)))
    }
  }

  protected def i_obscure_!(in : Seq[ENUM#Value]) = Nil

  private def st(in: Seq[ENUM#Value]) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String): (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(if (v eq null) defaultValue else fromLong(Helpers.toLong(v)))})

  def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(if (isNull) defaultValue else fromLong(v))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(if (v eq null) defaultValue else fromLong(Helpers.toLong(v)))})

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(if (v eq null) defaultValue else fromLong(Helpers.toLong(v)))})

  def buildSetBooleanValue(accessor : Method, columnName : String): (T, Boolean, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(defaultValue)})

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumListColumnType + notNullAppender()

  /**
   * Create an input field for the item
   */
  override def _toForm: Box[NodeSeq] =
  Full(SHtml.checkbox[ENUM#Value](enum.elements.toList, is,this(_)).toForm)
}

/**
 * Mix with MappedLong to give a default time of millis
 */
trait DefaultMillis extends TypedField[Long] {
  override def defaultValue = millis
}


class MappedNullableLong[T<:Mapper[T]](val fieldOwner: T) extends MappedNullableField[Long, T] {
  private var data: Box[Long] = defaultValue
  private var orgData: Box[Long] = defaultValue

  def defaultValue: Box[Long] = Empty
  def dbFieldClass = classOf[Box[Long]]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_! = data
  protected def i_was_! = orgData
  /**
   * Called after the field is saved to the database
   */
  override protected[mapper] def doneWithSave() {
    orgData = data
  }

  protected def real_i_set_!(value: Box[Long]): Box[Long] = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  def asJsExp = is.map(v => JE.Num(v)) openOr JE.JsNull

  override def readPermission_? = true
  override def writePermission_? = true

  def real_convertToJDBCFriendly(value: Box[Long]): Object = value match {
    case Full(value) => new _root_.java.lang.Long(value)
    case _ => null
  }

  // def asJsExp = JE.Num(is)

  def jdbcFriendly(field : String) = real_convertToJDBCFriendly(i_is_!)
  override def jdbcFriendly = real_convertToJDBCFriendly(i_is_!)

  override def setFromAny(in: Any): Box[Long] = {
    in match {
      case n: Long => this.set(Full(n))
      case n: Number => this.set(Full(n.longValue))
      case (n: Number) :: _ => this.set(Full(n.longValue))
      case Some(n: Number) => this.set(Full(n.longValue))
      case Full(n: Number) => this.set(Full(n.longValue))
      case Empty | Failure(_, _, _) => this.set(Empty)
      case None => this.set(Empty)
      case (s: String) :: _ => this.set(Helpers.asLong(s))
      case null => this.set(Empty)
      case s: String => this.set(Helpers.asLong(s))
      case o => this.set(Helpers.asLong(o))
    }
  }

  protected def i_obscure_!(in: Box[Long]) = defaultValue

  private def st(in: Box[Long]) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedNullableLong[T] => f.st(asLong(v))})

  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedNullableLong[T] => f.st(if (isNull) Empty else Full(v))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedNullableLong[T] => f.st(asLong(v))})

  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedNullableLong[T] => f.st(if (v == null) Empty else Full(v.getTime))})

  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longColumnType + notNullAppender()
}



class MappedLong[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Long, T] {
  private var data: Long = defaultValue
  private var orgData: Long = defaultValue

  def defaultValue: Long = 0L
  def dbFieldClass = classOf[Long]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_! = data
  protected def i_was_! = orgData
  /**
   * Called after the field is saved to the database
   */
  override protected[mapper] def doneWithSave() {
    orgData = data
  }

  protected def real_i_set_!(value : Long): Long = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  def asJsExp = JE.Num(is)

  override def readPermission_? = true
  override def writePermission_? = true

  def real_convertToJDBCFriendly(value: Long): Object = new _root_.java.lang.Long(value)

  // def asJsExp = JE.Num(is)

  def jdbcFriendly(field : String) = new _root_.java.lang.Long(i_is_!)
  override def jdbcFriendly = new _root_.java.lang.Long(i_is_!)

  override def setFromAny(in: Any): Long = {
    in match {
      case n: Long => this.set(n)
      case n: Number => this.set(n.longValue)
      case (n: Number) :: _ => this.set(n.longValue)
      case Some(n: Number) => this.set(n.longValue)
      case Full(n: Number) => this.set(n.longValue)
      case Empty | Failure(_, _, _) => this.set(0L)
      case None => this.set(0L)
      case (s: String) :: _ => this.set(toLong(s))
      case null => this.set(0L)
      case s: String => this.set(toLong(s))
      case o => this.set(toLong(o))
    }
  }

  protected def i_obscure_!(in : Long) = defaultValue

  private def st(in: Long) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedLong[T] => f.st(toLong(v))})

  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedLong[T] => f.st(if (isNull) defaultValue else v)})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedLong[T] => f.st(toLong(v))})

  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedLong[T] => f.st(if (v == null) defaultValue else v.getTime)})

  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longColumnType + notNullAppender()
}

