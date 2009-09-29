/*
 * LoggingStatementWrappers.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.liftweb.mapper

import _root_.java.io.{InputStream,Reader}
import _root_.java.net.URL
//import _root_.java.sql.{Blob,Clob,Connection,Date,NClob,PreparedStatement,Statement,Time,Timestamp}
import _root_.java.sql.{Array => SqlArray, _}
import _root_.java.util.Calendar

import _root_.net.liftweb.util.{Helpers}
import _root_.net.liftweb.base.{Box}

trait DBLogEntry {
  def statement : String
  def duration : Long
}
object DBLogEntry {
  def unapply(obj : Any) = obj match {
    case entry : DBLogEntry => Some(entry.statement,entry.duration)
    case _ => None
  }
}
case class DBStatementEntry(statement : String, duration : Long) extends DBLogEntry
case class DBMetaEntry(statement : String, duration : Long) extends DBLogEntry

/**
 * This trait is applied to JDBC statements and similar constructs that can log operations.
 *
 * To enable logging of DB operations, use DB.addLogFunc
 */
trait DBLog {
  protected var executedStatements = List[DBLogEntry]()

  /*
  Some convenience methods to simplify the statements. We defined methods that can either take a raw description,
  or a function that can use the result of the operation to construct a description.
  */
  protected def logStatement[T](description : String)(f : => T) : T = logStatement({ignore : T => description})(f)

  protected def logStatement[T](description : T => String)(f : => T) : T = Helpers.calcTime(f) match {
      case (duration, result) => executedStatements ::= DBStatementEntry(description(result), duration); result
  }

  protected def logMeta[T](description : String)(f : => T) : T = logMeta({ignore : T => description})(f)

  protected def logMeta[T](description : T => String)(f : => T) : T = Helpers.calcTime(f) match {
      case (duration, result) => executedStatements ::= DBMetaEntry(description(result), duration); result
  }

  /** Return a list of all of the DBStatementEntry instances in the log buffer */
  def statementEntries : List[DBStatementEntry] = executedStatements.filter(_.isInstanceOf[DBStatementEntry]).reverse.asInstanceOf[List[DBStatementEntry]]

  /** Return a list of all of the DBMetaEntry instances in the log buffer */
  def metaEntries : List[DBMetaEntry] = executedStatements.filter(_.isInstanceOf[DBMetaEntry]).reverse.asInstanceOf[List[DBMetaEntry]]

  /** Return all log buffer entries */
  def allEntries : List[DBLogEntry] = executedStatements.reverse
}

/**
 * This class corresponds to a logged version of java.sql.Statement. All operations
 * should be supported.
 *
 * To enable logging of DB operations, use DB.addLogFunc
 */
class LoggedStatement(underlying : Statement) extends Statement with DBLog {
  private val StatementClazz = classOf[Statement]

  // These are from wrapper and are required
  def isWrapperFor (clazz : Class[_]) : Boolean = clazz match {
      case StatementClazz => true
      case _ => underlying.isWrapperFor(clazz)
  }

  def unwrap[T] (clazz : Class[T]) : T = clazz match {
      case StatementClazz => underlying.asInstanceOf[T]
      case _ => underlying.unwrap(clazz)
  }

  def addBatch (sql : String) {
      logStatement("Batched: \"%s\"".format(sql)) {
        underlying.addBatch(sql)
      }
  }
  def cancel () {
      logMeta("Cancelled Statement") {
        underlying.cancel()
      }
  }
  def clearBatch () {
      logMeta("Cleared Batch") {
        underlying.clearBatch()
      }
  }
  def clearWarnings () {
      logMeta("Cleared Warnings") {
        underlying.clearWarnings()
      }
  }
  def close () {
      logMeta("Closed Statement") {
        underlying.close()
      }
  }
  def execute (sql : String) : Boolean = {
      logStatement({ret : Boolean => "\"%s\" : result = %s".format(sql, ret)}) {
          underlying.execute(sql)
      }
  }
  def execute (sql : String, autoKeys : Int) : Boolean = {
      logStatement({ret : Boolean => "Exec \"%s\", Auto-gen keys = %s : result = %s".format(sql, StatementConstantDescriptions.genKeyDescriptions(autoKeys), ret)}) {
        underlying.execute(sql, autoKeys)
      }
  }
  def execute (sql : String, autoKeyColumns : Array[Int]) : Boolean = {
      logStatement({ret : Boolean => "Exec \"%s\", Auto-gen keys for columns %s".format(sql, autoKeyColumns.mkString(", "), ret)}) {
          underlying.execute(sql, autoKeyColumns)
      }
  }
  def execute (sql : String, autoKeyColumns : Array[String]) : Boolean = {
      logStatement({ret : Boolean => "Exec \"%s\", Auto-gen keys for columns %s".format(sql, autoKeyColumns.mkString(", "))}) {
          underlying.execute(sql, autoKeyColumns)
      }
  }
  def executeBatch () : Array[Int] = {
      logStatement({result : Array[Int] => "Exec batch, counts = " + result.mkString("(", ", ", ")")}) {
          underlying.executeBatch()
      }
  }
  def executeQuery (sql : String) : ResultSet = {
      logStatement({rs : ResultSet => "Exec query \"%s\" : rs = %s".format(sql,rs)}) {
          underlying.executeQuery(sql)
      }
  }
  def executeUpdate (sql : String) : Int = {
      logStatement({count : Int => "Exec update \"%s\" : count = %d".format(sql,count)}) {
          underlying.executeUpdate(sql)
      }
  }
  def executeUpdate (sql : String, autoKeys : Int) : Int = {
      logStatement({count : Int => "Exec update \"%s\", Auto-gen keys = %s".format(sql, StatementConstantDescriptions.genKeyDescriptions(autoKeys), count)}) {
        underlying.executeUpdate(sql, autoKeys)
      }
  }
  def executeUpdate (sql : String, autoKeyColumns : Array[Int]) : Int = {
      logStatement({count : Int => "Exec update \"%s\", Auto-gen keys for columns %s".format(sql, autoKeyColumns.mkString(", "), count)}) {
        underlying.executeUpdate(sql, autoKeyColumns)
      }
  }
  def executeUpdate (sql : String, autoKeyColumns : Array[String]) : Int = {
      logStatement({count : Int => "Exec update \"%s\", Auto-gen keys for columns %s".format(sql, autoKeyColumns.mkString(", "), count)}) {
          underlying.executeUpdate(sql, autoKeyColumns)
      }
  }
  def getConnection () : Connection = {
      logMeta("Get underlying Connection") {
        underlying.getConnection
      }
  }
  def getFetchDirection () : Int = {
      logMeta({ret : Int => "Get fetch direction : " + StatementConstantDescriptions.fetchDirDescriptions(ret)}) {
          underlying.getFetchDirection()
      }
  }
  def getFetchSize () : Int = {
      logMeta({size : Int => "Get fetch size : " + size}) {
          underlying.getFetchSize()
      }
  }
  def getGeneratedKeys () : ResultSet = {
      logMeta({rs : ResultSet => "Get generated keys : rs = " + rs}) {
          underlying.getGeneratedKeys()
      }
  }
  def getMaxFieldSize () : Int = {
      logMeta({size : Int => "Get max field size : " + size}) {
          underlying.getMaxFieldSize()
      }
  }
  def getMaxRows () : Int = {
      logMeta({maxRows : Int => "Get max rows : " + maxRows}) {
          underlying.getMaxRows()
      }
  }
  def getMoreResults () : Boolean = {
      logMeta({hasMore : Boolean => "Get more results : " + hasMore}) {
          underlying.getMoreResults()
      }
  }
  def getMoreResults (current : Int) : Boolean = {
      logMeta({ret : Boolean => "Get more results (%s) : %s".format(StatementConstantDescriptions.getMoreResultsDescriptions(current), ret)}) {
          underlying.getMoreResults(current)
      }
  }
  def getQueryTimeout () : Int = {
      logMeta({timeout : Int => "Get query timeout : %d seconds ".format(timeout)}) {
          underlying.getQueryTimeout()
      }
  }
  def getResultSet () : ResultSet = {
      logMeta({rs : ResultSet => "Get result set : " + rs}) {
          underlying.getResultSet()
      }
  }
  def getResultSetConcurrency () : Int = {
      logMeta({ret : Int => "Get result set concurrency : " + StatementConstantDescriptions.resultSetConcurrencyDescs(ret)}) {
          underlying.getResultSetConcurrency()
      }
  }
  def getResultSetHoldability () : Int = {
      logMeta({ret : Int => "Get ResultSet holdability : " + StatementConstantDescriptions.resultSetHoldabilityDescs(ret)}) {
          underlying.getResultSetHoldability()
      }
  }
  def getResultSetType () : Int = {
      logMeta({ret : Int => "Get ResultSet type : " + StatementConstantDescriptions.resultSetTypeDescs(ret)}) {
          underlying.getResultSetType()
      }
  }
  def getUpdateCount () : Int = {
      logMeta({count : Int => "Get update count : " + count}) {
          underlying.getUpdateCount()
      }
  }
  def getWarnings () : SQLWarning = {
      logMeta({ret : SQLWarning => "Get SQL Warnings: " + Box.!!(ret).map(_.toString).openOr("None")}) {
          underlying.getWarnings()
      }
  }
  def isClosed () : Boolean = {
      logMeta({ret : Boolean => "Check isClosed : " + ret}) {
          underlying.isClosed()
      }
  }
  def isPoolable () : Boolean = {
      logMeta({ret : Boolean => "Check isPoolable : " + ret}) {
          underlying.isPoolable()
      }
  }
  def setCursorName (name : String) {
      logMeta("Set cursor name = %s" + name) {
          underlying.setCursorName(name)
      }
  }
  def setEscapeProcessing (enable : Boolean) {
      logMeta("Set escape processing = " + enable) {
          underlying.setEscapeProcessing(enable)
      }
  }
  def setFetchDirection (direction : Int) {
      logMeta("Set fetch direction = " + StatementConstantDescriptions.fetchDirDescriptions(direction)) {
          underlying.setFetchDirection(direction)
      }
  }
  def setFetchSize (size : Int) {
      logMeta("Set fetch size = " + size) {
          underlying.setFetchSize(size)
      }
  }
  def setMaxFieldSize (size : Int) {
      logMeta("Set max field size = " + size) {
          underlying.setMaxFieldSize(size)
      }
  }
  def setMaxRows (count : Int) {
      logMeta("Set max rows = " + count) {
          underlying.setMaxRows(count)
      }
  }
  def setPoolable (poolable : Boolean) {
      logMeta("Set poolable = " + poolable) {
          underlying.setPoolable(poolable)
      }
  }
  def setQueryTimeout (timeout : Int) {
      logMeta("Set query timeout = " + timeout) {
          underlying.setQueryTimeout(timeout)
      }
  }

  override def toString = executedStatements.reverse.mkString("\n")
}

/**
 * This class corresponds to a logged version of java.sql.PreparedStatement. All operations
 * should be supported.
 *
 * To enable logging of DB operations, use DB.addLogFunc
 */
class LoggedPreparedStatement (stmt : String, underlying : PreparedStatement) extends LoggedStatement(underlying) with PreparedStatement {
  private var paramMap = Map.empty[Int,Any]

  private val PreparedStatementClazz = classOf[PreparedStatement]

  // These are from wrapper and are required
  override def isWrapperFor (clazz : Class[_]) : Boolean = clazz match {
      case PreparedStatementClazz => true
      case _ => super.isWrapperFor(clazz) || underlying.isWrapperFor(clazz)
  }

  override def unwrap[T] (clazz : Class[T]) : T = clazz match {
      case PreparedStatementClazz => underlying.asInstanceOf[T]
      case _ => if (super.isWrapperFor(clazz)) super.unwrap(clazz) else underlying.unwrap(clazz)
  }

  // utility method to fill in params
  private def paramified : String = {
      def substitute (in : String, index : Int) : String = in.indexOf('?') match {
          case -1 => in
          case j => in.substring(0,j) + paramMap(index) + substitute(in.substring(j + 1), index + 1)
      }

      substitute(stmt, 1)
  }

  def addBatch () {
      logStatement("Batching \"%s\"".format(paramified)) {
          underlying.addBatch()
      }
  }

  def clearParameters () {
      logMeta("Clear parameters") {
          underlying.clearParameters()
      }
      paramMap = Map.empty[Int,Any]
  }

  def execute () : Boolean = {
      logStatement({ret : Boolean => "Exec \"%s\" : %s".format(paramified, ret)}) {
          underlying.execute()
      }
  }

  def executeQuery () : ResultSet = {
      logStatement({rs : ResultSet => "Exec query \"%s\" : %s".format(paramified, rs)}) {
          underlying.executeQuery()
      }
  }

  def executeUpdate () : Int = {
      logStatement({ret : Int => "Exec update \"%s\" : updated %d rows".format(paramified, ret)}) {
          underlying.executeUpdate()
      }
  }

  def getMetaData () : ResultSetMetaData = {
      logMeta({ret : ResultSetMetaData => "Get metadata : " + ret}) {
          underlying.getMetaData()
      }
  }

  def getParameterMetaData() : ParameterMetaData = {
      logMeta({ret : ParameterMetaData => "Get param metadata : " + ret}) {
          underlying.getParameterMetaData()
      }
  }

  def setArray (index : Int, x : SqlArray) {
      underlying.setArray(index, x)
      paramMap += index -> x
  }

  def setAsciiStream (index : Int, x : InputStream) {
      underlying.setAsciiStream(index, x)
      paramMap += index -> "(Ascii Stream: %s)".format(x)
  }

  def setAsciiStream (index : Int, x : InputStream, length : Int) {
      underlying.setAsciiStream(index, x, length)
      paramMap += index -> "(Ascii Stream: %s (%d bytes))".format(x, length)
  }

  def setAsciiStream (index : Int, x : InputStream, length : Long) {
      underlying.setAsciiStream(index, x, length)
      paramMap += index -> "(Ascii Stream: %s (%d bytes))".format(x, length)
  }

  def setBigDecimal (index : Int, x : java.math.BigDecimal) {
      underlying.setBigDecimal(index, x)
      paramMap += index -> x
  }

  def setBinaryStream (index : Int, x : InputStream) {
      underlying.setBinaryStream(index, x)
      paramMap += index -> "(Binary Stream: %s)".format(x)
  }

  def setBinaryStream (index : Int, x : InputStream, length : Int) {
      underlying.setBinaryStream(index, x, length)
      paramMap += index -> "(Binary Stream: %s (%d bytes))".format(x, length)
  }

  def setBinaryStream (index : Int, x : InputStream, length : Long) {
      underlying.setBinaryStream(index, x, length)
      paramMap += index -> "(Binary Stream: %s (%d bytes))".format(x, length)
  }

  def setBlob (index : Int, x : Blob) {
      underlying.setBlob(index, x)
      paramMap += index -> "(Blob : %s)".format(x)
  }

  def setBlob (index : Int, x : InputStream) {
      underlying.setBlob(index, x)
      paramMap += index -> "(Blob : %s)".format(x)
  }

  def setBlob (index : Int, x : InputStream, length : Long) {
      underlying.setBlob(index, x, length)
      paramMap += index -> "(Blob : %s (%d bytes))".format(x, length)
  }

  def setBoolean (index : Int, x : Boolean) {
      underlying.setBoolean(index, x)
      paramMap += index -> x
  }

  def setByte (index : Int, x : Byte) {
      underlying.setByte(index, x)
      paramMap += index -> x
  }

  def setBytes (index : Int, x : Array[Byte]) {
      underlying.setBytes(index, x)
      paramMap += index -> x
  }

  def setCharacterStream (index : Int, x : Reader) {
      underlying.setCharacterStream(index, x)
      paramMap += index -> "(Char stream : %s)".format(x)
  }

  def setCharacterStream (index : Int, x : Reader, length : Int) {
      underlying.setCharacterStream(index, x, length)
      paramMap += index -> "(Char stream : %s (%d bytes))".format(x, length)
  }

  def setCharacterStream (index : Int, x : Reader, length : Long) {
      underlying.setCharacterStream(index, x, length)
      paramMap += index -> "(Char stream : %s (%d bytes))".format(x, length)
  }

  def setClob (index : Int, x : Clob) {
      underlying.setClob(index, x)
      paramMap += index -> "(Clob : %s)".format(x)
  }

  def setClob (index : Int, x : Reader) {
      underlying.setClob(index, x)
      paramMap += index -> "(Clob : %s)".format(x)
  }

  def setClob (index : Int, x : Reader, length : Long) {
      underlying.setClob(index, x, length)
      paramMap += index -> "(Clob : %s (%d bytes))".format(x, length)
  }

  def setDate (index : Int, x : Date) {
      underlying.setDate(index, x)
      paramMap += index -> x
  }

  def setDate (index : Int, x : Date, cal : Calendar) {
      underlying.setDate(index, x, cal)
      paramMap += index -> (x + ":" + cal)
  }

  def setDouble (index : Int, x : Double) {
      underlying.setDouble(index, x)
      paramMap += index -> x
  }

  def setFloat (index : Int, x : Float) {
      underlying.setFloat(index, x)
      paramMap += index -> x
  }

  def setInt (index : Int, x : Int) {
      underlying.setInt(index, x)
      paramMap += index -> x
  }

  def setLong (index : Int, x : Long) {
      underlying.setLong(index, x)
      paramMap += index -> x
  }

  def setNCharacterStream (index : Int, x : Reader) {
      underlying.setNCharacterStream(index, x)
      paramMap += index -> "(NChar Stream : %s)".format(x)
  }

  def setNCharacterStream (index : Int, x : Reader, length : Long) {
      underlying.setNCharacterStream(index, x, length)
      paramMap += index -> "(NChar Stream : %s (%d bytes))".format(x, length)
  }

  def setNClob (index : Int, x : NClob) {
      underlying.setNClob(index, x)
      paramMap += index -> "(NClob : %s)".format(x)
  }

  def setNClob (index : Int, x : Reader) {
      underlying.setNClob(index, x)
      paramMap += index -> "(NClob : %s)".format(x)
  }

  def setNClob (index : Int, x : Reader, length : Long) {
      underlying.setNClob(index, x, length)
      paramMap += index -> "(NClob : %s (%d bytes))".format(x, length)
  }

  def setNString (index : Int, x : String) {
      underlying.setNString(index, x)
      paramMap += index -> x
  }

  def setNull (index : Int, sqlType : Int) {
      underlying.setNull(index, sqlType)
      paramMap += index -> "NULL"
  }

  def setNull (index : Int, sqlType : Int, typeName : String) {
      underlying.setNull(index, sqlType, typeName)
      paramMap += index -> "NULL"
  }

  def setObject (index : Int, x : Object) {
      underlying.setObject(index, x)
      paramMap += index -> x
  }

  def setObject (index : Int, x : Object, sqlType : Int) {
      underlying.setObject(index, x, sqlType)
      paramMap += index -> x
  }

  def setObject (index : Int, x : Object, sqlType : Int, scale : Int) {
      underlying.setObject(index, x, sqlType, scale)
      paramMap += index -> "%s (scale %d)".format(x, scale)
  }

  def setRef (index : Int, x : Ref) {
      underlying.setRef(index, x)
      paramMap += index -> x
  }

  def setRowId (index : Int, x : RowId) {
      underlying.setRowId(index, x)
      paramMap += index -> x
  }

  def setShort (index : Int, x : Short) {
      underlying.setShort(index, x)
      paramMap += index -> x
  }

  def setSQLXML (index : Int, x : SQLXML) {
      underlying.setSQLXML(index, x)
      paramMap += index -> x
  }

  def setString (index : Int, x : String) {
      underlying.setString(index, x)
      paramMap += index -> "\"%s\"".format(x)
  }

  def setTime (index : Int, x : Time) {
      underlying.setTime(index, x)
      paramMap += index -> x
  }

  def setTime (index : Int, x : Time, cal : Calendar) {
      underlying.setTime(index, x, cal)
      paramMap += index -> (x + ":" + cal)
  }

  def setTimestamp (index : Int, x : Timestamp) {
      underlying.setTimestamp(index, x)
      paramMap += index -> x
  }

  def setTimestamp (index : Int, x : Timestamp, cal : Calendar) {
      underlying.setTimestamp(index, x, cal)
      paramMap += index -> (x + ":" + cal)
  }

  def setUnicodeStream (index : Int, x : InputStream, length : Int) {
      underlying.setUnicodeStream(index, x, length)
      paramMap += index -> "(Unicode Stream : %s (%d bytes))".format(x, length)
  }

  def setURL (index : Int, x : URL) {
      underlying.setURL(index, x)
      paramMap += index -> "\"%s\"".format(x)
  }
}

/**
 * This object defines some conversions from Int JDBC constants to
 * descriptive strings
 */
object StatementConstantDescriptions {
    def genKeyDescriptions (in : Int) = in match {
        case Statement.NO_GENERATED_KEYS => "NO_GENERATED_KEYS"
        case Statement.RETURN_GENERATED_KEYS => "RETURN_GENERATED_KEYS"
        case x => "Invalid Generated Keys Constant: " + x
    }

    def fetchDirDescriptions (in : Int) = in match {
        case ResultSet.FETCH_FORWARD => "FETCH_FORWARD"
        case ResultSet.FETCH_REVERSE => "FETCH_REVERSE"
        case ResultSet.FETCH_UNKNOWN => "FETCH_UNKNOWN"
        case x => "Invalid Fetch Direction Constant: " + x
    }

    def getMoreResultsDescriptions (in : Int) = in match {
        case Statement.CLOSE_CURRENT_RESULT => "CLOSE_CURRENT_RESULT"
        case Statement.KEEP_CURRENT_RESULT => "KEEP_CURRENT_RESULT"
        case Statement.CLOSE_ALL_RESULTS => "CLOSE_ALL_RESULTS"
        case x => "Invalid getMoreResults constant: " + x
    }

    def resultSetConcurrencyDescs (in : Int) = in match {
        case ResultSet.CONCUR_READ_ONLY => "CONCUR_READ_ONLY"
        case ResultSet.CONCUR_UPDATABLE => "CONCUR_UPDATABLE"
        case x => "Invalid ResultSet concurrency constant: " + x
    }

    def resultSetHoldabilityDescs (in : Int) = in match {
        case ResultSet.HOLD_CURSORS_OVER_COMMIT => "HOLD_CURSORS_OVER_COMMIT"
        case ResultSet.CLOSE_CURSORS_AT_COMMIT => "CLOSE_CURSORS_AT_COMMIT"
        case x => "Invalid ResultSet holdability constant: " + x
    }

    def resultSetTypeDescs (in : Int) = in match {
        case ResultSet.TYPE_FORWARD_ONLY => "TYPE_FORWARD_ONLY"
        case ResultSet.TYPE_SCROLL_INSENSITIVE => "TYPE_SCROLL_INSENSITIVE"
        case ResultSet.TYPE_SCROLL_SENSITIVE => "TYPE_SCROLL_SENSITIVE"
        case x => "Invalid ResultSet type constant: " + x
    }
}
