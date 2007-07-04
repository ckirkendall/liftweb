package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.util.regex._

object MappedEmail {
  val emailPattern = Pattern.compile("^[a-z0-9._%-]+@(?:[a-z0-9-]+\\.)+[a-z]{2,4}$")
}

class MappedEmail[T<:Mapper[T]](owner : T, maxLen: int) extends MappedString[T](owner, maxLen) {

  override def setFilter = notNull _ :: toLower _ :: trim _ :: super.setFilter 
    
  override def validate = {
    (MappedEmail.emailPattern.matcher(i_get_!).matches match {
      case true => Nil
      case false => List(ValidationIssue(this, "Invalid Email Address"))
    }) ::: super.validate
  }
}
