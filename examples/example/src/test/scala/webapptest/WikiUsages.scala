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
package webapptest

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.net.sourceforge.jwebunit.junit.WebTester

class WikiUsagesTest extends Runner(WikiUsages) with JUnit with Console
object WikiUsages extends Specification {
  JettyTestServer.start()

  "wiki edit HomePage" should {
    setSequential

    "not have HomePage content define" >> {
      JettyTestServer.browse(
        "/wiki/HomePage",
        _.assertTextPresent("Create Entry named HomePage")
      )
    }

    "allow edition of HomePage" >> {
      JettyTestServer.browse(
        "/wiki/HomePage", wt => {
          var inputName = wt.getElementAttributByXPath("//textarea", "name")
          wt.setTextField(inputName, "hello test")
          //wt.clickButtonWithText("Add")
          wt.submit(wt.getElementAttributByXPath("//input[@value='Add']", "name"))
          wt.assertTextPresent("hello test")
          wt.clickLinkWithText("Edit")
          wt.assertTextPresent("Edit entry named HomePage")
          inputName = wt.getElementAttributByXPath("//textarea", "name")
          wt.setTextField(inputName, "bye test")
          //wt.clickButtonWithText("Add")
          wt.submit(wt.getElementAttributByXPath("//input[@value='Edit']", "name"))
          wt.assertTextPresent("bye test")
        }
      )
    }
  }

  //JettyTestServer.stop()
}

