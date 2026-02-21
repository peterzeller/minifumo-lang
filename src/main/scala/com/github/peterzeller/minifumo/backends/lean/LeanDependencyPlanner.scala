package com.github.peterzeller.minifumo.backends.lean

import scala.collection.mutable

object LeanDependencyPlanner:
  // Computes SCC groups and returns them in topological order.
  def topologicalSccs(graph: Map[String, Set[String]]): List[Set[String]] =
    val sccs = stronglyConnectedComponents(graph)
    val nodeToScc = sccs.zipWithIndex.flatMap((nodes, i) => nodes.map(_ -> i)).toMap
    val sccEdges = mutable.Map[Int, Set[Int]]().withDefaultValue(Set.empty)
    sccs.zipWithIndex.foreach: (nodes, i) =>
      val outgoing = nodes.flatMap(node => graph.getOrElse(node, Set.empty)).map(nodeToScc)
      sccEdges.update(i, outgoing - i)
    val inDegree = mutable.Map[Int, Int]().withDefaultValue(0)
    sccEdges.values.flatten.foreach(dst => inDegree.update(dst, inDegree(dst) + 1))
    val queue = mutable.PriorityQueue.empty[Int](using Ordering.Int.reverse)
    sccs.indices.filter(i => inDegree(i) == 0).foreach(queue.enqueue(_))
    val ordered = mutable.ListBuffer[Int]()
    while queue.nonEmpty do
      val i = queue.dequeue()
      ordered += i
      sccEdges(i).foreach: dst =>
        val next = inDegree(dst) - 1
        inDegree.update(dst, next)
        if next == 0 then queue.enqueue(dst)
    ordered.toList.map(sccs)

  // Runs Tarjan's algorithm to find SCCs in the graph.
  private def stronglyConnectedComponents(graph: Map[String, Set[String]]): List[Set[String]] =
    var index = 0
    val indices = mutable.Map[String, Int]()
    val lowLink = mutable.Map[String, Int]()
    val stack = mutable.Stack[String]()
    val onStack = mutable.Set[String]()
    val result = mutable.ListBuffer[Set[String]]()

    def connect(node: String): Unit =
      indices(node) = index
      lowLink(node) = index
      index += 1
      stack.push(node)
      onStack += node

      graph.getOrElse(node, Set.empty).foreach: dep =>
        if !indices.contains(dep) then
          connect(dep)
          lowLink(node) = math.min(lowLink(node), lowLink(dep))
        else if onStack.contains(dep) then
          lowLink(node) = math.min(lowLink(node), indices(dep))

      if lowLink(node) == indices(node) then
        val component = mutable.Set[String]()
        var done = false
        while !done do
          val item = stack.pop()
          onStack -= item
          component += item
          done = item == node
        result += component.toSet

    graph.keys.toList.sorted.foreach: node =>
      if !indices.contains(node) then
        connect(node)
    result.toList

