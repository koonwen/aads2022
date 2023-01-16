/**
 * Tries -- named so by their creator from the word "reTRIEval"
 *
 * Given an alphabet of N symbols (26 for the English alphabet), we build a tree where each node has N possible
 * children. Nodes do not contain the essential information here; edges are labelled by the symbols in the alphabet.
 * By loading a set of words into a Trie, we build a tree where the edges on each path from root to a leaf are
 * labeled by consecutive letters of a word in the set. Some valid words end before reaching a leaf (because
 * they may be prefixes of other longer words in the set -- e.g. 'ant' and 'anthem', so each node must have a flag
 * indicating whether the path from the root to the current node contains a valid word from the set.
 *
 * Possible applications of a Trie:
 *    - Set of words: determining whether a word is in the set or not.
 *    - Search index for words in a text. In this case, the node must be enriched to hold the positions in the
 *      text where the word can be found.
 *    - Search suggestions: Given a text and a trie containing an index for that text, as the user is typing in a
 *      search box, provide a list of possible completions for the current prefix typed in the search box.
 *    - Spellchecker: the Trie is loaded with words from a generic dictionary, and as one types, the text editor
 *      can provide feedback on whether the partial word being typed has a chance of being completed to a valid word.
 *    - Autocorrect while typing: build a trie having not only correct words, but also common misspellings of words;
 *      enrich the nodes to indicate whether the word ending at that node is correct or a misspelling,
 *      and in case it's a misspelling, indicate the common corrections.
 *    - String Sorting (BurstSort): Once the Trie is built, do a depth-first traversal to list the words of the trie
 *      in sorted order.
 *
 * The first 2 applications will be shown in class.
 * Search suggestions and BurstSort will be your assignment.
 * Autocorrect will be your second project.
 */

import collection.mutable

sealed trait Trie:
  val refs = mutable.Set.empty[Int]
  private[this] var _isTerminal: Boolean = false
  def isTerminal: Boolean = _isTerminal
  def makeTerminal(): Unit = _isTerminal = true
  def makeNonTerminal(): Unit = _isTerminal = false
  def isEmpty: Boolean
  def size: Int // number of words (i.e. number of terminal nodes)
  def depth: Int // useful for testing
  def insert(s: Seq[Char], ref: Option[Int] = None): Trie
  def search(s: Seq[Char]): (Boolean, Set[Int])

  def performDelete(s: Seq[Char], ref: Option[Int]): (Boolean, Trie) =
    val bResult = s.isEmpty && isTerminal && ref.forall(refs.contains)
    if bResult
    then
      if ref.isEmpty then refs.clear() else refs.remove(ref.get)
      if refs.isEmpty then makeNonTerminal()
    (bResult, this)

  def delete(s: Seq[Char], ref: Option[Int] = None): (Boolean, Trie)

case class EmptyTrie() extends Trie:
  def isEmpty: Boolean = ! isTerminal
  def size: Int = if isTerminal then refs.size else 0
  def depth: Int = 0
  def insert(s: Seq[Char], ref: Option[Int] = None): Trie = s match {
    case Seq() =>
      makeTerminal()
      ref.foreach(refs.add)
      this
    case c +: rest =>
      val newNode = TrieNode(Array.fill[Trie](26)(EmptyTrie()))
      if isTerminal
      then
        newNode.makeTerminal()
        newNode.refs.addAll(refs)
      newNode.edges(c-'a') = newNode.edges(c-'a').insert(rest, ref)
      newNode
  }
  def search(s: Seq[Char]): (Boolean, Set[Int]) = (s.isEmpty && isTerminal, refs.to(Set))

  def delete(s: Seq[Char], ref: Option[Int] = None): (Boolean, Trie) = performDelete(s, ref)

case class TrieNode(edges: Array[Trie]) extends Trie:
  def isEmpty: Boolean = !isTerminal && edges.forall(_.isEmpty)
  def size: Int = edges.map(_.size).sum + (if isTerminal then refs.size else 0)
  def depth: Int = edges.map(_.depth).max + 1
  def insert(s: Seq[Char], ref: Option[Int] = None): Trie =
    s match {
      case Seq() =>
        makeTerminal()
        ref.foreach(refs.add)
      case c +: rest =>
        edges(c-'a') = edges(c-'a').insert(rest, ref)
    }
    this
  def search(s: Seq[Char]): (Boolean, Set[Int]) = s match {
    case Seq() => (isTerminal, refs.to(Set))
    case c +: rest => edges(c - 'a').search(rest)
  }

  def makeNewNode: EmptyTrie =
    val newNode = EmptyTrie()
    if isTerminal then newNode.makeTerminal()
    newNode.refs.addAll(refs)
    newNode

  def delete(s: Seq[Char], ref: Option[Int] = None): (Boolean, Trie) = s match {
    case Seq() => performDelete(s, ref)
    case c +: rest =>
      edges(c - 'a').delete(rest, ref) match {
        case (true, t) =>
          edges(c - 'a') = t
          (true, if ! edges.exists(n => n.isTerminal || n.isInstanceOf[TrieNode]) then makeNewNode else this)
        case _ => (false, this)
      }
  }

class StringSet(private[this] var t: Trie):
  def isEmpty: Boolean = t.isEmpty
  def size: Int = t.size
  def depth: Int = t.depth
  def insert(s: String): Unit = t = t.insert(s.to(Seq))
  def contains(s: String): Boolean = t.search(s.to(Seq))._1
  def delete(s: String): Unit = t = t.delete(s.to(Seq))._2

object StringSet:
  def apply(): StringSet = new StringSet(EmptyTrie())

class StringIndex(private[this] var t: Trie):
  def size: Int = t.size
  def insert(s: String, ref: Int): Unit = t = t.insert(s, Option(ref))
  def find(s: String): Set[Int] = t.search(s.to(Seq))._2

  def delete(s: String, ref: Int): Boolean =
    val result = t.delete(s.to(Seq), Option(ref))
    t = result._2
    result._1

object StringIndex:
  def apply(): StringIndex = new StringIndex(EmptyTrie())