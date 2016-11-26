/* Copyright (c) 2016 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gnieh.easysync
package client

import model._

import akka.actor._

import scala.concurrent.duration.Duration

import scala.reflect.ClassTag

import com.typesafe.config.ConfigFactory

/** The client is only responsible for synchronization between server and view.
 *  It receives updates from both and notifies the view when changes come from the server.
 *
 *  The client sends changesets to the `view` when it is connected and when
 *  changes are received from the server from other clients.
 *  It is up to the `view` to treat these changeset (updating their content, caret position, ...)
 */
class Client[C: ClassTag](view: ActorRef, server: ActorRef) extends Actor {

  val changesetTag = implicitly[ClassTag[Changeset[C]]]
  val documentTag = implicitly[ClassTag[Document[C]]]

  def receive = connecting()

  server ! Connect

  // schedule the submission messages
  val submitInterval = Duration.fromNanos(ConfigFactory.load.getDuration("akka.easysync.client.submit-interval").toNanos)
  val submissionTask =
    context.system.scheduler.schedule(submitInterval, submitInterval, self, Submit)(context.system.dispatcher)

  def connecting(): Receive = {

    case HeadRevision(rev, documentTag(doc)) =>
      // we are now connected

      val changeset = Changeset.fromDocument(doc)
      view ! changeset

      context.become(connected(changeset, identity(doc.size), identity(doc.size)))

  }

  def connected(acknowledged: Changeset[C], unacknowledged: Changeset[C], local: Changeset[C]): Receive = {

    case LocalEdit(changesetTag(cs)) =>
      // receive a new local edit from the view
      val local1 = local.andThen(cs)

      context.become(connected(acknowledged, unacknowledged, local1))

    case Submit =>
      // submit new changeset to the server if no unacknowledged changes are pending
      // and there are local changes
      val id = identity(acknowledged.to)
      if (unacknowledged == id) {
        if (local != id) {

          server ! NewChangeset(local)

          context.become(connected(acknowledged, local, identity(local.to)))

        }
      }

    case Ack =>
      // server acknowledges the sent changeset
      val acknowledged1 = acknowledged.andThen(unacknowledged)
      context.become(connected(acknowledged1, identity(acknowledged1.to), local))

    case DocumentRevision(changesetTag(changeset), author) =>
      // hear about another client’s changeset
      val acknowledged1 = acknowledged.andThen(changeset)
      val unacknowledged1 = follow(changeset, unacknowledged)
      val local1 = follow(follow(unacknowledged, changeset), local)

      val viewcs = follow(local, follow(unacknowledged, changeset))

      view ! viewcs

      context.become(connected(acknowledged1, unacknowledged1, local1))

    case Stop =>

      submissionTask.cancel()

      context.stop(self)

  }

}
