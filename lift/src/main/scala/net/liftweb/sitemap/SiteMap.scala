package net.liftweb.sitemap

/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
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

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Helpers._

import _root_.scala.xml.{NodeSeq}

class SiteMapException(msg: String) extends Exception(msg)

case class SiteMap(kids: Menu*) extends HasKids  {
  private var locs: Map[String, Loc] = Map.empty

  kids.foreach(_._parent = Full(this))
  kids.foreach(_.init(this))
  kids.foreach(_.validate)
  private[sitemap] def addLoc(in: Loc) {
    if (locs.isDefinedAt(in.name)) 
    throw new SiteMapException("Location "+in.name+" defined twice "+
    locs(in.name)+" and "+in)
    else locs = locs + (in.name -> in)
  }

  def findLoc(name: String): Can[Loc] =
  Can(locs.get(name))

  def findLoc(req: RequestState): Can[Loc] =
    first(kids)(_.findLoc(req))

  def locForGroup(group: String): Seq[Loc] = 
  kids.flatMap(_.locForGroup(group)).filter(_.testAccess match {
    case Left(true) => true case _ => false})
}

object SiteMap {
  def findLoc(name: String): Can[Loc] =
  for (sm <- LiftRules.siteMap;
       loc <- sm.findLoc(name)) yield loc

  def findAndTestLoc(name: String): Can[Loc] =
  findLoc(name).flatMap(l => l.testAccess match {
      case Left(true) => Full(l)
      case _ => Empty
    })

  def buildLink(name: String, text: NodeSeq): NodeSeq =
  for (loc <- findAndTestLoc(name).toList;
       link <- loc.link.createLink(Nil))
  yield <a href={link}>{
      text match {
        case x if x.text.length > 0 => x
        case _ => loc.text.text()
      }
    }</a>

  def buildLink(name: String): NodeSeq =
  buildLink(name, Nil)
}

trait HasKids {
  def kids: Seq[Menu]
  def buildUpperLines(pathAt: HasKids, actual: Menu, populate: List[MenuItem]): List[MenuItem] 
  = populate
  
  def isRoot_? = false

  private[sitemap] def testAccess: Either[Boolean, Can[LiftResponse]] = Left(true)
}
