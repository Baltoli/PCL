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

  trait Process extends ParseTree.Node
  case class OutProcess(chan: ParseTree.Name, expr: ParseTree.Expression) extends Process
  case class InProcess(chan: ParseTree.Name, varName: ParseTree.VariableName) extends Process
  case class ParallelProcess(left: Process, right: Process) extends Process
  case class ReplicateProcess(proc: Process) extends Process
  case class IfProcess(left: ParseTree.Expression, right: ParseTree.Expression, proc: Process) extends Process
  case class LetProcess(name: ParseTree.VariableName, value: ParseTree.Expression, proc: Process) extends Process
  case class EndProcess() extends Process

  private case class ParseTreeSequentialInternalProcess(first: ParseTree.InternalProcess,
                                                        second: ParseTree.InternalProcess) extends ParseTree.InternalProcess

  private def rebalance(tree: ParseTree.Node): ParseTree.Node = {
    tree match {
      case ParseTree.ProcessStart(p) =>
        ParseTree.ProcessStart(rebalanceProcess(p))

      case _ => tree
    }
  }

  private def rebalanceProcess(tree: ParseTree.InternalProcess): ParseTree.InternalProcess = {
    val stripped = removeProcessAux(tree)
    tree match {
      case ParseTree.AugmentedInProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.AugmentedOutProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.ParallelInternalProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.ReplicateInternalProcess(_, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.IfInternalProcess(_, _, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParseTree.LetInternalProcess(_, _, _, aux) => sequenceIfNeeded(stripped, aux)
      case _ => stripped
    }
  }

  private def sequenceIfNeedearseTree.AugmentedProcess, aux: ParseTree.ProcearseTree.AugmentedProcess = {
    processFromAux(aux) match {
      case Some(next) => ParseTreeSequentialInternalProcess(proc, rebalanceProcess(next))
      case None => proc
    }
  }

  private def processFromAux(aux: ParseTree.ProcessAux):arseTree.AugmentedProcess] = {
    aux match {
      case ParseTree.SequentialProcessAux(proc, _) => Some(proc)
      case ParseTree.EmptyProcessAux() => None
    }
  }

  private def removeProcessAuarseTree.AugmentedPrarseTree.AugmentedProcess = {
    val empty = ParseTree.EmptyProcessAux()
    proc match {
      case ParseTree.AugmentedInProcess(c, v, _) => ParseTree.AugmentedInProcess(c, v, empty)
      case ParseTree.AugmentedOutProcess(c, e, _) => ParseTree.AugmentedOutProcess(c, e, empty)
      case ParseTree.ParallelInternalProcess(l, r, _) => ParseTree.ParallelInternalProcess(l, r, empty)
      case ParseTree.ReplicateInternalProcess(p, _) => ParseTree.ReplicateInternalProcess(p, empty)
      case ParseTree.IfInternalProcess(l, r, p, _) => ParseTree.IfInternalProcess(l, r, p, empty)
      case ParseTree.LetInternalProcess(n, v, p, _) => ParseTree.LetInternalProcess(n, v, p, empty)
      case _ => proc
    }
  }

}
