import com.github.peterzeller.minifumo.typing.kernel.Kernel
import com.github.peterzeller.minifumo.typing.kernel.Kernel.{ConstDecl, Context, Env, Reducibility, Term}

class KernelSuite extends munit.FunSuite {
  import Term.*

  /** Builds a non-dependent function type A -> B in the current context. */
  private def arrow(dom: Term, cod: Term): Term = Pi(dom, Kernel.lift(cod, 1, 0))

  /** Constructs the core environment with basic constants for typing tests. */
  private def baseEnv: Env = {
    val natDecl = ConstDecl(Sort(0), None, Reducibility.Opaque)
    val boolDecl = ConstDecl(Sort(0), None, Reducibility.Opaque)
    val trueDecl = ConstDecl(Const("Bool"), None, Reducibility.Opaque)
    val falseDecl = ConstDecl(Const("Bool"), None, Reducibility.Opaque)
    val threeDecl = ConstDecl(Const("Nat"), None, Reducibility.Opaque)
    val oneDecl = ConstDecl(Const("Nat"), None, Reducibility.Opaque)

    val idType = Pi(Sort(0), Pi(Var(0), Var(1)))
    val idValue = Lam(Sort(0), Lam(Var(0), Var(0)))
    val idDecl = ConstDecl(idType, Some(idValue), Reducibility.Reducible)

    val kType = Pi(Sort(0), Pi(Sort(0), arrow(Var(1), arrow(Var(0), Var(1)))))
    val kValue = Lam(Sort(0), Lam(Sort(0), Lam(Var(1), Lam(Var(1), Var(1)))))
    val kDecl = ConstDecl(kType, Some(kValue), Reducibility.Reducible)

    val compType =
      Pi(
        Sort(0),
        Pi(
          Sort(0),
          Pi(
            Sort(0),
            Pi(
              arrow(Var(1), Var(0)),
              Pi(
                arrow(Var(3), Var(2)),
                arrow(Var(4), Var(2))
              )
            )
          )
        )
      )

    val compValue =
      Lam(
        Sort(0),
        Lam(
          Sort(0),
          Lam(
            Sort(0),
            Lam(
              arrow(Var(1), Var(0)),
              Lam(
                arrow(Var(3), Var(2)),
                Lam(
                  Var(4),
                  App(Var(2), App(Var(1), Var(0)))
                )
              )
            )
          )
        )
      )

    Env.empty
      .addConstant("Nat", natDecl)
      .addConstant("Bool", boolDecl)
      .addConstant("true", trueDecl)
      .addConstant("false", falseDecl)
      .addConstant("three", threeDecl)
      .addConstant("one", oneDecl)
      .addConstant("id", idDecl)
      .addConstant("K", kDecl)
      .addConstant("comp", ConstDecl(compType, Some(compValue), Reducibility.Reducible))
  }

  test("lift by zero is identity") {
    val term = Lam(Sort(0), App(Var(0), Var(1)))
    assertEquals(Kernel.lift(term, 0, 0), term)
  }

  test("subst shifts replacement under binders") {
    val term = Lam(Sort(0), Var(1))
    val result = Kernel.subst(term, 0, Var(2))
    assertEquals(result, Lam(Sort(0), Var(3)))
  }

  test("beta reduces identity") {
    val term = App(Lam(Sort(0), Var(0)), Const("a"))
    val result = Kernel.whnf(Env.empty, Context.empty, term)
    assertEquals(result, Const("a"))
  }

  test("beta reduces nested lambdas") {
    val term = App(App(Lam(Sort(0), Lam(Sort(0), Var(1))), Const("a")), Const("b"))
    val result = Kernel.whnf(Env.empty, Context.empty, term)
    assertEquals(result, Const("a"))
  }

  test("beta avoids variable capture") {
    val body = Lam(Sort(0), Var(1))
    val reduced = Kernel.betaReduce(body, Var(0))
    assertEquals(reduced, Lam(Sort(0), Var(1)))
  }

  test("whnf reduces let bindings") {
    val term = Let(Const("a"), Const("A"), Var(0))
    val result = Kernel.whnf(Env.empty, Context.empty, term)
    assertEquals(result, Const("a"))
  }

  test("whnf unfolds reducible constants") {
    val env = Env.empty.addConstant(
      "def",
      ConstDecl(Const("T"), Some(Lam(Sort(0), Var(0))), Reducibility.Reducible)
    )
    val result = Kernel.whnf(env, Context.empty, Const("def"))
    assertEquals(result, Lam(Sort(0), Var(0)))
  }

  test("whnf does not reduce under lambdas") {
    val term = Lam(Sort(0), App(Lam(Sort(0), Var(0)), Const("a")))
    val result = Kernel.whnf(Env.empty, Context.empty, term)
    assertEquals(result, term)
  }

  test("conv respects beta conversion") {
    val t1 = App(Lam(Sort(0), Var(0)), Const("a"))
    val t2 = Const("a")
    assert(Kernel.conv(Env.empty, Context.empty, t1, t2))
  }

  test("conv respects delta conversion") {
    val env = Env.empty.addConstant(
      "def",
      ConstDecl(Const("T"), Some(Const("body")), Reducibility.Reducible)
    )
    assert(Kernel.conv(env, Context.empty, Const("def"), Const("body")))
  }

  test("conv compares lambdas under binders") {
    val t1 = Lam(Sort(0), Lam(Sort(0), Var(1)))
    val t2 = Lam(Sort(0), Lam(Sort(0), Var(1)))
    assert(Kernel.conv(Env.empty, Context.empty, t1, t2))
  }

  test("infer id type") {
    val env = baseEnv
    val inferred = Kernel.infer(env, Context.empty, Const("id"))
    assert(Kernel.conv(env, Context.empty, inferred, Pi(Sort(0), Pi(Var(0), Var(1)))))
  }

  test("infer id Nat three") {
    val env = baseEnv
    val term = App(App(Const("id"), Const("Nat")), Const("three"))
    val inferred = Kernel.infer(env, Context.empty, term)
    assert(Kernel.conv(env, Context.empty, inferred, Const("Nat")))
  }

  test("K Nat Bool one true has type Nat") {
    val env = baseEnv
    val term = App(App(App(App(Const("K"), Const("Nat")), Const("Bool")), Const("one")), Const("true"))
    Kernel.check(env, Context.empty, term, Const("Nat"))
  }

  test("comp with id specializes correctly") {
    val env = baseEnv
    val idNat = App(Const("id"), Const("Nat"))
    val term =
      App(
        App(
          App(
            App(
              App(
                App(Const("comp"), Const("Nat")),
                Const("Nat")
              ),
              Const("Nat")
            ),
            idNat
          ),
          idNat
        ),
        Const("three")
      )
    val inferred = Kernel.infer(env, Context.empty, term)
    assert(Kernel.conv(env, Context.empty, inferred, Const("Nat")))
  }

  test("let binds value and types correctly") {
    val env = baseEnv
    val term = Let(Const("three"), Const("Nat"), Var(0))
    val inferred = Kernel.infer(env, Context.empty, term)
    assert(Kernel.conv(env, Context.empty, inferred, Const("Nat")))
  }
}
