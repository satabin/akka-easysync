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
package server

import model._

import akka.actor._
import akka.persistence._

class Server(projectId: String, docId: String) extends PersistentActor {

  def persistenceId = f"akka-easysync:$projectId:$docId"

  def started(clients: Map[ActorPath, Int], head: Document[Char], revisions: Vector[DocumentRevision], minRev: Int): Receive = {

    case Connect =>

      val rev = revisions.size + minRev

      // send the current revision to the client
      sender ! HeadRevision(rev, head)

      // add the client to the map with its last know revision
      context.become(started(clients.updated(sender.path, rev), head, revisions, minRev))

    case NewChangeset(changeset) =>
      clients.get(sender.path) match {
        case Some(clientRev) =>

          // capture the sender to be used in the callback
          val s = sender

          // compute the changeset in the context of the current revision
          // first retrieve the last known changeset
          // short-circuit: if the last know revision is the last revision then nothing
          // to do
          val changeset1 =
            if (clientRev >= revisions.size + minRev - 1)
              changeset
            else
              revisions.view(clientRev + 1 - minRev, revisions.size).foldLeft(changeset) {
                case (changeset1, DocumentRevision(c, _)) =>
                  follow(c, changeset1)
              }

          val rev = DocumentRevision(changeset, s.path.name)

          // persist the changeset event
          persistAsync(rev) { rev =>
            // create the revision out of sender and changeset

            // notify the other clients about this new revision and get the mimimum revision
            val minRev1 = clients.foldLeft(Int.MaxValue) {
              case (min, (p, r)) =>
                if (p != s.path)
                  context.actorSelection(p) ! rev
                math.min(min, r)
            }

            val revisions1 =
              if (minRev1 > minRev) {
                // we can remove all the revisions before this new minimum revision
                revisions.drop(minRev1 - minRev)
              } else {
                revisions
              }

            // send ack to the original sender
            s ! Ack

            context.become(started(clients.updated(s.path, revisions1.size + minRev1 + 1), changeset(head), revisions1 :+ rev, minRev1))
          }

        case None =>
        // this client is not connected, just ignore its message
      }

  }

  val receiveRecover: Receive = recovering(Document.empty, Vector(DocumentRevision(Changeset.empty, "")), 0)

  def recovering(head: Document[Char], revisions: Vector[DocumentRevision], minRev: Int): Receive = {
    case msg @ (Connect | NewChangeset(_)) =>
      stash()
    case SnapshotOffer(_, ServerState(document, revision)) =>
      context.become(recovering(document, Vector(DocumentRevision(Changeset(0, Seq(Sequence(document.characters))), "")), revision))
    case rev @ DocumentRevision(cs, _) =>
      context.become(recovering(cs(head), revisions :+ rev, minRev))
    case RecoveryCompleted =>
      unstashAll()
      context.become(started(Map.empty, head, revisions, minRev))
  }

  val receiveCommand: Receive = started(Map.empty, Document.empty, Vector(DocumentRevision(Changeset.empty, "")), 0)

}