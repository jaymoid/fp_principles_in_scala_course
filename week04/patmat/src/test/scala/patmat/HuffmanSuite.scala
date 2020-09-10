package patmat

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit._

class HuffmanSuite {

  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    /*  t2...
              (a,b,d) 9
              /     \
          (a,b) 5   (d) 4
         /     \
       (a) 2   (b) 3
     */
  }

  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }

  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a', 'b', 'd'), chars(t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `times returns no of occurances of chars`: Unit =
    assertEquals(times(List('a', 'b', 'a')), List(('a', 2), ('b', 1)))

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))

  @Test def `singleton returns true for lists with a single codeTree in`: Unit = {
    assertTrue(singleton(List(Leaf('e', 1))))
    assertFalse(singleton(List()))
    assertFalse(singleton(List(Leaf('e', 1), Leaf('f', 1))))
  }


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)), combine(leaflist))
  }

  @Test def `decode an encoding`: Unit = {
    val a = List(0, 0)
    val b = List(0, 1)
    val d = List(1)

    new TestTrees {
      assertEquals("babddab".toList, decode(t2, b ++ a ++ b ++ d ++ d ++ a ++ b))
    }
  }

  @Test def `encode a message`: Unit = {
    val a = List(0, 0)
    val b = List(0, 1)
    val d = List(1)

    new TestTrees {
      assertEquals(b ++ a ++ b ++ d ++ d ++ a ++ b, encode(t2)("babddab".toList))

      assertEquals(b ++ a ++ b ++ d ++ d ++ a ++ b, quickEncode(t2)("babddab".toList))
    }
  }


  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `create a CodeTree` =
    println(createCodeTree("Hello World".toList))


  @Test def `what is the french secret?`: Unit =
    println(decodedSecret.mkString)


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
