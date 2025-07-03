package com.computationalcognitivescience.coherencecommunication.util

/**
 * Temporary until features are implemented in mathlib.
 */
object SetTheoryDev {
  // TODO Replace with mathlib import when update is published.
  def forall[A](set: Set[A], f: A => Boolean): Boolean = set.forall(f)
  def exists[A](set: Set[A], f: A => Boolean): Boolean = set.exists(f)
}
