/*
 * Copyright 2024 SiriusXM
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

package snapshot4s

import org.scalacheck.Gen
import weaver._
import weaver.scalacheck._

object ApplyPatchSpec extends SimpleIOSuite with Checkers {

  /** Original implementation using start-to-end with offset tracking */
  def applyPatchWithOffset(source: String, patches: List[(Int, Int, String)]): String =
    patches
      .sortBy(_._1)
      .foldLeft((source, 0))((acc, patch) => {
        val (source, offset)           = acc
        val (startPos, endPos, middle) = patch
        val start                      = source.take(offset + startPos)
        val end                        = source.drop(offset + endPos)
        val nextSource                 = start ++ middle ++ end
        val lengthIncrease             = middle.length - (endPos - startPos)
        val nextOffset                 = offset + lengthIncrease
        (nextSource, nextOffset)
      })
      ._1

  /** Simplified implementation using end-to-start (no offset needed) */
  def applyPatchEndToStart(source: String, patches: List[(Int, Int, String)]): String =
    patches
      .sortBy(-_._1)
      .foldLeft(source) { (source, patch) =>
        val (startPos, endPos, middle) = patch
        source.take(startPos) ++ middle ++ source.drop(endPos)
      }

  val sourceGen: Gen[String] = Gen.stringOfN(100, Gen.alphaNumChar)

  def patchGen(sourceLength: Int): Gen[(Int, Int, String)] =
    for {
      start       <- Gen.choose(0, math.max(0, sourceLength - 1))
      end         <- Gen.choose(start, sourceLength)
      replacement <- Gen.stringOf(Gen.alphaNumChar)
    } yield (start, end, replacement)

  def nonOverlappingPatchesGen(sourceLength: Int): Gen[List[(Int, Int, String)]] =
    for {
      numPatches <- Gen.choose(0, 5)
      patches    <- Gen.listOfN(numPatches, patchGen(sourceLength))
      sorted      = patches.sortBy(_._1)
      nonOverlapping = sorted.foldLeft(List.empty[(Int, Int, String)]) { (acc, patch) =>
        acc.lastOption match {
          case Some((_, prevEnd, _)) if patch._1 < prevEnd => acc
          case _                                           => acc :+ patch
        }
      }
    } yield nonOverlapping

  pureTest("single patch replacement") {
    val source  = "Hello, world!"
    val patches = List((7, 12, "Scala"))
    val result  = applyPatchEndToStart(source, patches)
    expect.eql(result, "Hello, Scala!")
  }

  pureTest("single patch at start") {
    val source  = "Hello, world!"
    val patches = List((0, 5, "Hi"))
    val result  = applyPatchEndToStart(source, patches)
    expect.eql(result, "Hi, world!")
  }

  pureTest("single patch at end") {
    val source  = "Hello, world!"
    val patches = List((7, 13, "Scala!"))
    val result  = applyPatchEndToStart(source, patches)
    expect.eql(result, "Hello, Scala!")
  }

  pureTest("multiple patches - both implementations produce same result") {
    val source  = "The quick brown fox jumps over the lazy dog"
    val patches = List(
      (4, 9, "slow"),    // "quick" -> "slow"
      (16, 19, "cat"),   // "fox" -> "cat"
      (35, 39, "sleepy") // "lazy" -> "sleepy"
    )
    val offsetResult     = applyPatchWithOffset(source, patches)
    val endToStartResult = applyPatchEndToStart(source, patches)
    expect.eql(offsetResult, endToStartResult) and
      expect.eql(endToStartResult, "The slow brown cat jumps over the sleepy dog")
  }

  pureTest("patches that grow the string") {
    val source  = "ab"
    val patches = List(
      (0, 1, "aaa"), // "a" -> "aaa"
      (1, 2, "bbb")  // "b" -> "bbb"
    )
    val offsetResult     = applyPatchWithOffset(source, patches)
    val endToStartResult = applyPatchEndToStart(source, patches)
    expect.eql(offsetResult, endToStartResult) and
      expect.eql(endToStartResult, "aaabbb")
  }

  pureTest("patches that shrink the string") {
    val source  = "aaabbb"
    val patches = List(
      (0, 3, "a"), // "aaa" -> "a"
      (3, 6, "b")  // "bbb" -> "b"
    )
    val offsetResult     = applyPatchWithOffset(source, patches)
    val endToStartResult = applyPatchEndToStart(source, patches)
    expect.eql(offsetResult, endToStartResult) and
      expect.eql(endToStartResult, "ab")
  }

  pureTest("empty patches list") {
    val source           = "unchanged"
    val patches          = List.empty[(Int, Int, String)]
    val offsetResult     = applyPatchWithOffset(source, patches)
    val endToStartResult = applyPatchEndToStart(source, patches)
    expect.eql(offsetResult, endToStartResult) and
      expect.eql(endToStartResult, source)
  }

  pureTest("insertion (start == end)") {
    val source           = "Hello world"
    val patches          = List((5, 5, ","))
    val offsetResult     = applyPatchWithOffset(source, patches)
    val endToStartResult = applyPatchEndToStart(source, patches)
    expect.eql(offsetResult, endToStartResult) and
      expect.eql(endToStartResult, "Hello, world")
  }

  pureTest("deletion (empty replacement)") {
    val source           = "Hello, world"
    val patches          = List((5, 6, ""))
    val offsetResult     = applyPatchWithOffset(source, patches)
    val endToStartResult = applyPatchEndToStart(source, patches)
    expect.eql(offsetResult, endToStartResult) and
      expect.eql(endToStartResult, "Hello world")
  }

  pureTest("patches provided in reverse order") {
    val source  = "abcdef"
    val patches = List(
      (4, 6, "EF"), // provided first but later in string
      (0, 2, "AB")  // provided second but earlier in string
    )
    val offsetResult     = applyPatchWithOffset(source, patches)
    val endToStartResult = applyPatchEndToStart(source, patches)
    expect.eql(offsetResult, endToStartResult) and
      expect.eql(endToStartResult, "ABcdEF")
  }

  pureTest("patches provided in random order") {
    val source  = "0123456789"
    val patches = List(
      (6, 8, "XX"),  // middle position
      (0, 2, "AA"),  // start position
      (8, 10, "ZZ")  // end position
    )
    val offsetResult     = applyPatchWithOffset(source, patches)
    val endToStartResult = applyPatchEndToStart(source, patches)
    expect.eql(offsetResult, endToStartResult) and
      expect.eql(endToStartResult, "AA2345XXZZ")
  }

  test("property: both implementations produce identical results for random non-overlapping patches") {
    forall(sourceGen.flatMap(s => nonOverlappingPatchesGen(s.length).map((s, _)))) {
      case (source, patches) =>
        val offsetResult     = applyPatchWithOffset(source, patches)
        val endToStartResult = applyPatchEndToStart(source, patches)
        expect.eql(offsetResult, endToStartResult)
    }
  }

  test("property: order of patches in input list doesn't affect result") {
    forall(sourceGen.flatMap(s => nonOverlappingPatchesGen(s.length).map((s, _)))) {
      case (source, patches) =>
        val result1 = applyPatchEndToStart(source, patches)
        val result2 = applyPatchEndToStart(source, patches.reverse)
        val result3 = applyPatchEndToStart(source, scala.util.Random.shuffle(patches))
        expect.eql(result1, result2) and expect.eql(result2, result3)
    }
  }

}
