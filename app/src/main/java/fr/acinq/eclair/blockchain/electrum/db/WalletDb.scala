/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.blockchain.electrum.db

import fr.acinq.bitcoin.{BlockHeader, ByteVector32}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.PersistentData
import fr.acinq.eclair.CltvExpiry

trait HeaderDb {
  def addHeaders(startHeight: Int, headers: Seq[BlockHeader] = Nil): Unit
  def getHeader(height: Int): Option[BlockHeader]

  type HeightAndHeader = (Int, BlockHeader)
  def getHeader(blockHash: ByteVector32): Option[HeightAndHeader]
  def getHeaders(startHeight: Int, maxCount: Int): Seq[BlockHeader]
  def getTip: Option[HeightAndHeader]
}

trait WalletDb extends HeaderDb {
  def persist(data: PersistentData): Unit
  def readPersistentData: Option[PersistentData]
}