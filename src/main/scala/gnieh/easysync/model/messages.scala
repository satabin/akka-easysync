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
package model

import scala.reflect.ClassTag

/** Send a new changeset to the server. Expects a [[Ack]] as response.
 *
 *  @group Request
 */
final case class NewChangeset(changeset: Changeset[Char])

/** The current head revision of the document.
 *
 *  @group Response
 */
final case class HeadRevision(rev: Int, doc: Document[Char])

/** A new document revision with changes and author information.
 *
 *  @group Response
 */
final case class DocumentRevision(changeset: Changeset[Char], author: String)

/** Connect to the server. Expects a [[HeadRevision]] as response.
 *
 *  @group Request
 */
case object Connect

/** Disconnect from the server. Expects no response.
 *
 *  @group Request
 */
case object Disconnect

/** Request the document to be saved to store. Expects no response.
 *
 *  @group Request
 */
case object Save

/** Server acknowlegment of received changeset.
 *
 *  @group Response
 */
case object Ack
