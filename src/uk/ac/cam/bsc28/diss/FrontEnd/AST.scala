package uk.ac.cam.bsc28.diss.FrontEnd

/*
 * The AST structure is designed to reflect the abstract semantics of
 * the program it represents, rather than the grammatical structure of
 * the parsed sequence of tokens. For some languages the parse tree and
 * the AST may be identical (and indeed, here they are very similar). The
 * difference comes from the elimination of left recursion from the
 * grammar - we are left with a grammar that recognizes the same set of
 * strings, but provides differently structured 'witnesses' (e.g. output
 * trees).
 *
 * In essence, what we aim to do at this step is rework the parse tree so
 * that the non-left recursive structure (which looks like a long chain
 * style tree) is replaced by a binary branching tree.
 */

object AST {

  private case class ParseTreeSequentialProcess(first: ParseTree.Process,
                                                second: ParseTree.Process) extends ParseTree.Process

  def rebalance(tree: ParseTree.Node): ParseTree.Node = {
    tree match {
      case ParseTree.ProcessStart(p) =>
        ParseTree.ProcessStart(rebalanceProcess(p))

      case _ => tree
    }
  }

  private def rebalanceProcess(tree: ParseTree.Process): ParseTree.Process = {
    val stripped = removeProcessAux(tree)
    tree match {
      case ParseTree.InProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.OutProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.ParallelProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.ReplicateProcess(_, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.IfProcess(_, _, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.LetProcess(_, _, _, aux) => sequenceIfNeeded(stripped, aux)
      case _ => stripped
    }
  }

  private def sequenceIfNeeded(proc: ParseTree.Process, aux: ParseTree.ProcessAux): ParseTree.Process = {
    processFromAux(aux) match {
      case Some(next) => ParseTreeSequentialProcess(proc, rebalanceProcess(next))
      case None => proc
    }
  }

  private def processFromAux(aux: ParseTree.ProcessAux): Option[ParseTree.Process] = {
    aux match {
      case ParseTree.SequentialProcessAux(proc, _) => Some(proc)
      case ParseTree.EmptyProcessAux() => None
    }
  }

  private def removeProcessAux(proc: ParseTree.Process): ParseTree.Process = {
    val empty = ParseTree.EmptyProcessAux()
    proc match {
      case ParseTree.InProcess(c, v, _) => ParseTree.InProcess(c, v, empty)
      case ParseTree.OutProcess(c, e, _) => ParseTree.OutProcess(c, e, empty)
      case ParseTree.ParallelProcess(l, r, _) => ParseTree.ParallelProcess(l, r, empty)
      case ParseTree.ReplicateProcess(p, _) => ParseTree.ReplicateProcess(p, empty)
      case ParseTree.IfProcess(l, r, p, _) => ParseTree.IfProcess(l, r, p, empty)
      case ParseTree.LetProcess(n, v, p, _) => ParseTree.LetProcess(n, v, p, empty)
      case _ => proc
    }
  }

}
