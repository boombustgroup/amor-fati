package com.boombustgroup.amorfati.engine

/** Strong month types for the month-by-month engine boundary.
  *
  * `CompletedMonth` lives on the persistent simulation boundary state and
  * counts how many months have already been closed.
  *
  * `ExecutionMonth` is the month currently being realized by one engine step.
  */
object SimulationMonth:

  opaque type CompletedMonth = Int
  object CompletedMonth:
    val Zero: CompletedMonth = 0

    def apply(value: Int): CompletedMonth =
      require(value >= 0, s"CompletedMonth must be >= 0, got $value")
      value

    extension (month: CompletedMonth)
      inline def toInt: Int   = month
      inline def toLong: Long = month.toLong

      def next: ExecutionMonth =
        ExecutionMonth(month + 1)

      def advanceBy(delta: Int): CompletedMonth =
        require(delta >= 0, s"CompletedMonth delta must be >= 0, got $delta")
        CompletedMonth(month + delta)

    given Ordering[CompletedMonth] = Ordering.Int

  opaque type ExecutionMonth = Int
  object ExecutionMonth:
    val First: ExecutionMonth = 1

    def apply(value: Int): ExecutionMonth =
      require(value >= 1, s"ExecutionMonth must be >= 1, got $value")
      value

    extension (month: ExecutionMonth)
      inline def toInt: Int   = month
      inline def toLong: Long = month.toLong

      def completed: CompletedMonth =
        CompletedMonth(month)

      def previousCompleted: CompletedMonth =
        CompletedMonth(month - 1)

      def next: ExecutionMonth =
        ExecutionMonth(month + 1)

      def advanceBy(delta: Int): ExecutionMonth =
        require(delta >= 0, s"ExecutionMonth delta must be >= 0, got $delta")
        ExecutionMonth(month + delta)

      def monthInYear: Int =
        ((month - 1) % 12) + 1

    given Ordering[ExecutionMonth] = Ordering.Int
