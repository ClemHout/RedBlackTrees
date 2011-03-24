/*
 *    Author : Clement Houtmann
 *    Copyright (C) 2011 Clement Houtmann
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License along
 *    with this program; if not, write to the Free Software Foundation, Inc.,
 *    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */



package RedBlackTree

class RedBlackTree[E <% Ordered[E]] {

  var tree : RBTree[E] = new Leaf()
  val manipulator = new RBTreeManipulator[E]

  def insert(value : E) = {
    tree = manipulator insert(value, tree)
  }

}


// red black trees
abstract class Color
case class Red() extends Color
case class Black() extends Color

abstract class RBTree[E]

case class Leaf[E]() extends RBTree[E]
case class Node[E](value: E, color:Color, left: RBTree[E], right: RBTree[E]) extends RBTree[E]

abstract class Direction
case class Left() extends Direction
case class Right() extends Direction

case class ZipperNode[E](direction: Direction, value: E, color: Color, left: RBTree[E])

class RBTreeManipulator[E <% Ordered[E]] {

  val black = new Black()
  val red = new Red()
  var goleft = new Left()
  val goright = new Right()
  val leaf : Leaf[E] = new Leaf()

  def ZipperNode(dir: Direction, value: E, color: Color, tree: RBTree[E]) : ZipperNode[E] = {
    new ZipperNode(dir, value: E, color: Color, tree: RBTree[E])
  }

  def Node(value:E, color: Color, left: RBTree[E], right: RBTree[E]) = {
    new Node(value:E, color: Color, left: RBTree[E], right: RBTree[E])
  }

  def paintitblack(tree : RBTree[E]) = tree match {
    case Leaf() => tree
    case Node(value, color, left, right) => Node(value, black, left, right)
  }

  def unzip(zip:List[ZipperNode[E]], tree : RBTree[E]) : RBTree[E] = zip match {
    case List() => tree
    case ZipperNode(Left() , value, color, left) :: sublist => unzip(sublist, Node(value, color, left, tree))
    case ZipperNode(Right(), value, color, right) :: sublist => unzip(sublist, Node(value, color, tree, right))
  }

  def insertZipperAux(value: E, tree: RBTree[E], zip: List[ZipperNode[E]]) : List[ZipperNode[E]] = tree match {
    case Leaf() =>  zip
    case Node(value2, color, left, right) =>
      if (value < value2) insertZipperAux(value, left, ZipperNode(goright , value2, color, right) :: zip)
      else insertZipperAux(value, right, ZipperNode(goleft, value2, color, left) :: zip)
  }

  def insertZipper(value: E, tree: RBTree[E]) = insertZipperAux(value, tree, List())

  def correctZipper(zip:List[ZipperNode[E]], tree : RBTree[E]) : RBTree[E] = zip match {
    // case 0 : no parent -> the node is root and we just paint it black
    case List() => paintitblack(tree)
    // case 1 : the parent is black -> the tree is valid
    case ZipperNode(dir, value, Black(), subtree) :: sublist => unzip(zip, tree)
    // case 2 : parent and uncle are red -> we paint them both black and continue with granpa
    case ZipperNode(dir, value, Red(), subtree) ::
          ZipperNode(dir2, value2, Black(), Node(value3, Red(), left, right)) ::
            sublist =>
              correctZipper(sublist, unzip(
                ZipperNode(dir, value, black, subtree) ::
                ZipperNode(dir2, value2, red, Node(value3, black, left, right)) :: List(),
                tree
              ))
    // case 3 : parent red, uncle black, node on the right, parent on the left
    case ZipperNode(Left(), value, Red(), subtree) ::
          ZipperNode(Right(), value2, Black(), blackuncle) ::
            sublist => tree match {
      // perform rotation
      case Node(value4, Red(), left1, right1) => correctZipper(
        ZipperNode(goright, value4, red, right1) ::
          ZipperNode(goright, value2, black, blackuncle) ::
            sublist,
        Node(value, red, subtree, left1))
      }
    // case 4 : parent red, uncle black, node on the left, parent on the left
    case ZipperNode(Right(), value, Red(), subtree) ::
          ZipperNode(Right(), value2, Black(), blackuncle) ::
            sublist => unzip(sublist,
          Node(value, black, tree, Node(value2, red, subtree, blackuncle))
        )
    // case 4prime : parent red, uncle black, node on the right, parent on the right
    case ZipperNode(Left(), value, Red(), subtree) ::
          ZipperNode(Left(), value2, Black(), blackuncle) ::
            sublist => unzip(sublist,
          Node(value, black, Node(value2, red, blackuncle, subtree), tree))
    // case 3prime : parent red, uncle black, node on the left, parent on the right
    case ZipperNode(Right(), value, Red(), subtree) ::
          ZipperNode(Left(), value2, Black(), blackuncle) ::
            sublist => tree match {
      // perform rotation
      case Node(value4, Red(), left1, right1) => correctZipper(
        ZipperNode(goleft, value4, red, left1) ::
          ZipperNode(goleft, value2, black, blackuncle) ::
            sublist,
        Node(value, red, right1, subtree)
        )
      }
    // failure -> should not happen... replace with exception ?
    case _ => println("Warning: probably uncorrect R/B tree"); unzip(zip, tree)
  }

  def insert(value: E, tree: RBTree[E]) = {
    correctZipper(insertZipper(value, tree), Node(value, red, leaf, leaf))
  }

}
