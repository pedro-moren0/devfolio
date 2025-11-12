+++
date = '2024-06-17T00:00:00-03:00'
draft = false
title = 'Representando programas com GADTs et al. em Haskell'
tags = ['Haskell', 'Interpretadores', 'Tipos de segunda ordem']
+++

*Por brevidade, algumas funções não foram incluídas no texto deste artigo. Para o código desse tutorial na íntegra, acesse o [repositório do GitHub](https://github.com/pedro-moren0/dgt-tutorial2).*

---

No nosso [último tutorial](../progrepr-adts), utilizamos *Algebraic Data Structures* do Haskell para representar uma *árvore de sintaxe abstrata* (AST) de um programa. Fomos bem sucedidos e conseguimos implementar uma função para avaliar essa AST. Mesmo assim algumas coisas não ficaram legais com o nosso código.

O primeiro ponto é a falta de segurança quando usamos uma função. Em nenhum momento é feita a checagem na quantidade de argumentos passados, ou nos tipos dos argumentos, o que pode ser visto nas expressões abaixo:

```haskell
-- | Uma expressao com erro no tipo do parametro
exprErr1 :: AST
exprErr1 =
  If
    -< [ IsDigit
           -< [Leaf $ Literal (IntL 1)], -- esperava um CharL
         SubInt
           -< [ AddInt
                  -< [ Leaf $ Literal (IntL 2),
                       MultInt
                         -< [ Leaf $ Literal (IntL 5),
                              Leaf $ Literal (IntL 3)
                            ]
                     ],
                Leaf $ Literal (IntL 7)
              ],
         ToUpper
           -< [Leaf $ Literal (CharL 'c')]
       ]

-- | Uma expressao com erro na quantidade de parametros
exprErr2 :: AST
exprErr2 =
  If
    -< [ IsDigit
           -< [Leaf $ Literal (CharL '1')],
         SubInt
           -< [ AddInt
                  -< [ Leaf $ Literal (IntL 2),
                       MultInt
                         -< [ Leaf $ Literal (IntL 5),
                              Leaf $ Literal (IntL 3)
                            ]
                     ],
                Leaf $ Literal (IntL 7), -- SubInt tem aridade 2
                Leaf $ Literal (IntL 7),
                Leaf $ Literal (IntL 7),
                Leaf $ Literal (IntL 7)
              ],
         ToUpper
           -< [Leaf $ Literal (CharL 'c')]
       ]
```

Isso nos leva a ficar muito dependentes da checagem de erros feita em `eval` em tempo de execução. Somos forçados a criar um caso nessa função que é um `fallback` para erros inesperados. O que nos leva ao próximo problema: ter um caso `fallback` tira a possibilidade do GHC nos dizer que tem algum caso que não implementamos. Por exemplo, no outro código não foi possível perceber que a operação `DivInt` não tinha sido implementada.

Somado a isso, quando tratamos de *input* do usuário via argumentos, não há forma de validar se o índice fornecido é valido.

Por fim, para que o nosso código funcione, são necessárias várias funções pouco acopladas. Isso é ruim, pois temos que estar atentos a modificações que possivelmente quebram o código em vários pontos diferentes.

Dessa forma, vamos propor alguns objetivos a fim de tornar nosso programa mais robusto:

1. Escrever nossas funções de forma que não seja possível violar a aridade definida, nem violar os tipos de argumentos esperados por elas.
2. Validar os índices de acesso aos argumentos fornecidos pelos usuários de maneira que eles sejam sempre válidos.
3. Validar o tipo do argumento passado pelo usuário em relação ao que a função espera.
4. Remover casos *fallback* da função `eval`.

## Melhorando as nossas funções

O primeiro objetivo é relativamente mais fácil de se alcançar: podemos usar a extensão de linguagem `GADTs` do Haskell, que permite escrever os nossos ADTs de uma forma muito mais robusta. Primeiro vamos começar reescrevendo alguns dos nossos literais. A principal mudança aqui é que iremos parametrizar `Lit` com um tipo genérico `a`. Faremos isso para garantir que uma expressão literal retorne o tipo que ela carrega. Modifique o código em `Grammar.Core`

```haskell
{-# LANGUAGE GADTs #-}

data Lit a where
  IntL :: Int -> Lit Int
  FloatL :: Float -> Lit Float
  CharL :: Char -> Lit Char
  BoolL :: Bool -> Lit Bool
  PairL :: Lit b -> Lit c -> Lit (b, c)
  ListL :: [Lit a] -> Lit [a]
```

Com o GADTs habilitado, tipos soma são representados cada um em uma linha diferente, e tipos produtos são representados com essa assinatura idêntica a assinatura que especifica funções. Veja que por causa dessa extensão, um `IntL` não é qualquer `Lit` , mas sim um `Lit Int`! Esse construtor se tornou bem mais específico, e isso vai nos ajudar muito a garantir a integridade dos tipos nas nossas ASTs.

Vamos implementar também algumas das operações do último tutorial. Elas também serão parametrizadas pelo tipo de retorno:

```haskell
data Op a where
  C :: Lit a -> Op a
  AddInt :: Op Int -> Op Int -> Op Int
  SubInt :: Op Int -> Op Int -> Op Int
  MultInt :: Op Int -> Op Int -> Op Int
  DivInt :: Op Int -> Op Int -> Op Int
  ModInt :: Op Int -> Op Int -> Op Int
  GTEInt :: Op Int -> Op Int -> Op Bool
  LTEInt :: Op Int -> Op Int -> Op Bool
  Equals :: (Eq a) => Op a -> Op a -> Op Bool
  AddFloat :: Op Float -> Op Float -> Op Float
  SubFloat :: Op Float -> Op Float -> Op Float
  MultFloat :: Op Float -> Op Float -> Op Float
  DivFloat :: Op Float -> Op Float -> Op Float
  Sqrt :: Op Float -> Op Float
  IsLetter :: Op Char -> Op Bool
  IsDigit :: Op Char -> Op Bool
  ToUpper :: Op Char -> Op Char
  ToLower :: Op Char -> Op Char
  And :: Op Bool -> Op Bool -> Op Bool
  Or :: Op Bool -> Op Bool -> Op Bool
  Not :: Op Bool -> Op Bool
  If :: Op Bool -> Op a -> Op a -> Op a
```

Aqui podemos ver a utilidade de parametrizar os literais: podemos passar o tipo paramétrico em frente e gerar uma operação própria para aquele tipo.

Essa representação por si só já garante o nosso primeiro objetivo. Cada função deixa de ser só um nome e agora passa ter uma assinatura associada, garantindo a aridade dela. Além disso, restringe também quais operações podem ser passadas para outras operações como parâmetros, pois agora somos obrigados a casar os tipos. É isso que queríamos.

Outra consequência disso é que uma estrutura arbórea nasce da própria recursividade do tipo `Op`. Dessa maneira, a definição de `AST` que tínhamos antes passa a ser obsoleta. Por semântica, vamos apelidar `Op` de `AST` e usar um ou outro a depender do contexto que parecer mais adequado.

```haskell
type AST a = Op a
```

Vamos tentar representar `exprErr1` e `exprErr2` com essa nova sintaxe

```haskell
exprErr1 :: AST Int
exprErr1 =
  If
    (IsDigit (C (IntL 1)))
    ( SubInt
        ( AddInt
            (C (IntL 2))
            ( MultInt
                (C (IntL 5))
                (C (IntL 3))
            )
        )
        (C (IntL 7))
    )
    (C (IntL (-1)))
```

Imediatamente o compilador reclama

```
• Couldn't match type ‘Int’ with ‘Char’
      Expected: Lit Char
        Actual: Lit Int
    • In the first argument of ‘C’, namely ‘(IntL 1)’
      In the first argument of ‘IsDigit’, namely ‘(C (IntL 1))’
      In the first argument of ‘If’, namely ‘(IsDigit (C (IntL 1)))’
   |
95 |     (IsDigit (C (IntL 1)))
   |                  ^^^^^^
```

Era o que queríamos! No outro código esse erro teria passado batido e teria que ser resolvido em tempo de execução. Agora o compilador impede esse tipo de situação.

```haskell
exprErr2 :: AST Int
exprErr2 =
  If
    (IsDigit (C (CharL 'c')))
    ( SubInt
        ( AddInt
            (C (IntL 2))
            ( MultInt
                (C (IntL 5))
                (C (IntL 3))
            )
        )
        (C (IntL 7))
        (C (IntL 7))
        (C (IntL 7))
        (C (IntL 7))
    )
    (C (IntL (-1)))
```

Mais uma vez o compilador nos salva, como vemos abaixo. Agora ele reclama da quantidade de argumentos aplicados à `SubInt`. Isso resolve o problema de aplicar mais argumentos que o possível em uma função. O mesmo tipo de erro seria apresentado se tivéssemos passado argumentos de menos.

```
• Couldn't match expected type: Op Int -> Op Int -> Op Int -> Op Int
                  with actual type: Op Int
    • The function ‘SubInt’ is applied to five value arguments,
        but its type ‘Op Int -> Op Int -> Op Int’ has only two
      In the second argument of ‘If’, namely
        ‘(SubInt
            (AddInt (C (IntL 2)) (MultInt (C (IntL 5)) (C (IntL 3))))
            (C (IntL 7)) (C (IntL 7)) (C (IntL 7)) (C (IntL 7)))’
      In the expression:
        If
          (IsDigit (C (CharL 'c')))
          (SubInt
             (AddInt (C (IntL 2)) (MultInt (C (IntL 5)) (C (IntL 3))))
             (C (IntL 7)) (C (IntL 7)) (C (IntL 7)) (C (IntL 7)))
          (C (IntL (- 1)))
   |
96 |     ( SubInt
   |       ^^^^^^...
```

Ok, parece que para o primeiro objetivo, alcançamos os resultados desejados só habilitando uma extensão. Entretanto, não conseguimos representar funções de alta ordem nem argumentos de entrada nessas funções. Vamos implementar essas funcionalidades.

## Validando argumentos do usuário

No código passado, acessávamos um argumento fornecido pelo usuário passando um `Int` ao construtor `Arg`. Mas isso não nos dava garantia nenhuma: poderíamos escrever `Arg (-1)`, ou `Arg 9001` para uma lista de argumentos de tamanho 1, e o nosso programa compilaria. É esse tipo de situação que procuramos evitar.

Também seria desejável associar um tipo ao parâmetro passado por um usuário, já que não queremos lidar com erros de *type mismatch*. Por exemplo, se `Arg 0` é do tipo a `Char`, eu gostaria de ser impedido de escrever algo como `AddInt (Arg 0) (C (IntL 1))`.

Para resolver esses dois problemas, decidi modelar uma lista de argumentos como uma lista heterogênea, como visto em [aula](https://haskell.pesquisa.ufabc.edu.br/desenvolvimento-orientado-a-tipos/06.typefamily/#lista-heterog%C3%AAnea). A sua definição é

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Args.HList (module Args.HList) where

import Data.Kind (Type)

type HList :: [Type] -> Type
data HList xs where
  Nil :: HList '[]
  (:.) :: x -> HList xs -> HList (x ': xs)

infixr 5 :.
```

Sem me extender muito, as extensões usadas para esse código compilar, além do `GADTs`, são o `DataKinds`, que promove todos *data constructors* para tipos, e todos os *type constructors* (TC) para *kinds*, e o `TypeOperators`, que permite que usemos símbolos como TCs. O `StandaloneKindSignatures` não é obrigatório, mas ele permite anotar os *kinds* da mesma maneira que anotamos tipos. Outro ponto é que usamos `Type` ou invés de `*` para representar o *kind type*.

O motivo dessa escolha é que o `DataKinds` nos permite escrever listas de tipos, e isso se assemelha muito com uma assinatura de função, já que o tipo `'[Int]` é diferente do tipo `'[Int, Int, Int]`.

Para representar os índices de acesso a essa lista, usei a biblioteca `TypeNats`, que eleva os números naturais a nível de tipo. Essa biblioteca também implementa *singleton types* para esses números, o que nos permite transitar entre os níveis dos tipos e o nível dos valores por conta da bijeção entre o tipo e o valor.

Explicadas essas escolhas, estamos prontos para implementar uma operação de acesso aos argumentos! Primeiramente, crie um módulo chamado `Args` na pasta `src`, e dentro dele crie o arquivo `HList.hs`. É lá que iremos guardar a nossa definição de lista heterogênea.

Uma mudança no código que escrevemos é que agora as operações vão carregar também a assinatura da lista de argumentos. Essa lista precisa ser passada pois, em qualquer lugar da nossa AST, podemos invocar o valor de um argumento, essa informação tem que estar à mão. Aplicando essas mudanças ficamos com o seguinte código (também adicionamos mais algumas assinaturas para funções com pares e listas):

```haskell
import GHC.TypeNats

type Op :: [Type] -> Type -> Type
data Op xs a where
  A :: (KnownNat n, n < Len xs) => SNat n -> Op xs (xs ! n)
  C :: Lit a -> Op xs a
  AddInt :: Op xs Int -> Op xs Int -> Op xs Int
  SubInt :: Op xs Int -> Op xs Int -> Op xs Int
  MultInt :: Op xs Int -> Op xs Int -> Op xs Int
  DivInt :: Op xs Int -> Op xs Int -> Op xs Int
  ModInt :: Op xs Int -> Op xs Int -> Op xs Int
  GTEInt :: Op xs Int -> Op xs Int -> Op xs Bool
  LTEInt :: Op xs Int -> Op xs Int -> Op xs Bool
  Equals :: (Eq a) => Op xs a -> Op xs a -> Op xs Bool
  AddFloat :: Op xs Float -> Op xs Float -> Op xs Float
  SubFloat :: Op xs Float -> Op xs Float -> Op xs Float
  MultFloat :: Op xs Float -> Op xs Float -> Op xs Float
  DivFloat :: Op xs Float -> Op xs Float -> Op xs Float
  Sqrt :: Op xs Float -> Op xs Float
  IsLetter :: Op xs Char -> Op xs Bool
  IsDigit :: Op xs Char -> Op xs Bool
  ToUpper :: Op xs Char -> Op xs Char
  ToLower :: Op xs Char -> Op xs Char
  And :: Op xs Bool -> Op xs Bool -> Op xs Bool
  Or :: Op xs Bool -> Op xs Bool -> Op xs Bool
  Not :: Op xs Bool -> Op xs Bool
  If :: Op xs Bool -> Op xs a -> Op xs a -> Op xs a
  ToPair :: Op xs a -> Op xs b -> Op xs (a, b)
  Fst :: Op xs (a, b) -> Op xs a
  Snd :: Op xs (a, b) -> Op xs b
  Len :: Op xs [a] -> Op xs Int
  Cons :: Op xs a -> Op xs [a] -> Op xs [a]
  Head :: Op xs [a] -> Op xs a
  Tail :: Op xs [a] -> Op xs [a]
```

`A` representa o nosso argumento. Naturalmente essa assinatura não compila, pois temos que implementar as *type families* (TFs) necessárias, que permitem com que criemos funções no nível dos tipos. Crie outro arquivo em `Args` chamado `FamilyUtils.hs`. Vamos concentrar nossas TFs aqui.

```haskell
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Args.FamilyUtils (module Args.FamilyUtils) where

import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeNats (Nat, type (+), type (-))

type Len :: [Type] -> Nat
type family Len a where
  Len '[] = 0
  Len (_ : xs) = 1 + Len xs

type (<?) :: Nat -> Nat -> Bool
type family n <? m where
  n <? 0 = 'False
  0 <? m = 'True
  n <? m = (n - 1) <? (m - 1)

type (<) :: Nat -> Nat -> Constraint
type family n < m where
  n < m = n <? m ~ 'True

type (!) :: [Type] -> Nat -> Type
type family xs ! n where
  '[] ! _ = Void
  (x ': _) ! 0 = x
  (_ ': xs) ! n = xs ! (n - 1)
  ```

Aqui já conseguimos esclarecer alguns pontos da assinatura de `A` . O primeiro deles é `n < Len xs` . De maneira bem direta, estamos dizendo que `A` só segue se conseguimos garantir que o `n`, o valor do índice, é menor que o tamanho da assinatura da função. Isso garante o nosso problema de acesso com índices maiores que o permitido acabe. Como estamos tratando de naturais também, números negativos não são possíveis de se construir.

Agora, para garantir o tipo, criamos uma função de acesso `(!)`, análoga a função que usamos para acessar elementos numa lista em Haskell. O motivo disso é que, se a assinatura da minha função é `'[Int, Bool, Int]`, e eu estou acessando a posição `2`, eu quero que `A 2 :: Op '[Int, Bool, Int] Int` para que os tipos casem dentro da AST. No caso da lista de argumentos ser vazia, independente do índice, eu quero que o tipo retornado seja o tipo inabitado `Void` , pelo motivo que é impossível retornar qualquer valor justamente por ele ser vazio. Mesmo que alguém declare uma `AST _ Void`, não existe implementação possível para essa assinatura, o que nos dá a garantia de nunca conseguir fazer um acesso a uma lista vazia. Nos demais casos, subtraímos recursivamente do valor de `n` até chegar no caso base quando ele é 0.

Finalmente, precisamos garantir a condição `KnownNat n` para conseguir construir o nosso índice representado por um `SNat`. Para fazer isso, temos que habilitar a extensão `TypeApplications` e escrever `natSing @n`, para que possamos gerar um `SNat n`.

Exemplos:

```haskell
x :: AST '[Int, Bool] Int
x = AddInt (A (natSing @0)) (C (IntL 1)) -- compila!

-- Mas...
x :: AST '[Int, Bool] Int
x = AddInt (A (natSing @1)) (C (IntL 1)) -- nao compila...
```

O compilador nos informa que os tipos não casam:

```
• Couldn't match type ‘Bool’ with ‘Int’
      Expected: Op [Int, Bool] Int
        Actual: Op [Int, Bool] ([Int, Bool] Args.FamilyUtils.! 1)
    • In the first argument of ‘AddInt’, namely ‘(A (natSing @1))’
      In the expression: AddInt (A (natSing @1)) (C (IntL 1))
      In an equation for ‘x’: x = AddInt (A (natSing @1)) (C (IntL 1))
   |
93 | x = AddInt (A (natSing @1)) (C (IntL 1))
   |
```

Perfeito! No caso de acesso fora dos limites da lista…

```haskell
x :: AST '[Int, Bool] Int
x = AddInt (A (natSing @2)) (C (IntL 1)) -- nao compila...
```

---

```
• Couldn't match type ‘GHC.Base.Void’ with ‘Int’
      Expected: Op [Int, Bool] Int
        Actual: Op [Int, Bool] ([Int, Bool] Args.FamilyUtils.! 2)
    • In the first argument of ‘AddInt’, namely ‘(A (natSing @2))’
      In the expression: AddInt (A (natSing @2)) (C (IntL 1))
      In an equation for ‘x’: x = AddInt (A (natSing @2)) (C (IntL 1))
   |
93 | x = AddInt (A (natSing @2)) (C (IntL 1))
   | 
```

Se tentarmos representar uma AST que retorna `Void` também não conseguiremos

```haskell
x :: AST '[] Void
x = A (natSing @0) -- nao compila
```

---

```
• Couldn't match type ‘False’ with ‘True’ arising from a use of ‘A’
    • In the expression: A (natSing @0)
      In an equation for ‘x’: x = A (natSing @0)
   |
94 | x = A (natSing @0)
   | 
```

Tudo parece estar perfeito! Muito perfeito…

## Executando nossas ASTs

Primeiramente vamos criar um novo apelido para o `HList`. Anote no final do arquivo de `HList.hs`:

```haskell
type Args :: [Type] -> Type
type Args xs = HList xs
```

Queremos que a assinatura da nossa função eval seja algo assim

```haskell
type ST a = Either String a

eval :: AST xs a -> Args xs -> ST a
```

Vamos receber uma AST que retorna um `a`, e uma lista de argumentos com assinatura `xs` e retornaremos ou um valor de `a`, ou uma `String` que carrega o nosso *stacktrace*.

Para a maior parte das funções, a implementação de `eval` é muito direta. Como `Either` é uma mônada, podemos avaliar cada operação “dentro da mônada” com o `do` *notation*, combinar os valores com os operadores usuais do Haskell, e embrulhar o resultado novamente na mônada. No caso das operações que falham, retornamos uma mensagem de erro. Por exemplo,

```haskell
eval (MultFloat x1 x2) ls = do -- ls eh a lista de argumentos fornecida
  n <- eval x1 ls
  m <- eval x2 ls
  return $ n * m
eval (DivFloat x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  if m == 0 then Left "Division by zero" else return $ n / m
```

Agora, como vamos avaliar um argumento `A`?

```haskell
-- relembrando...
-- A :: (KnownNat n, n < Len xs) => SNat n -> Op xs (xs ! n)

eval (A sn) ls = ???
```

Temos que implementar, da mesma forma que fizemos a nível de tipos, uma função de acesso a uma `HList`, mas agora a nível de valores. Uma primeira tentativa poderia ser algo como o seguinte: dada uma lista heterogênea não vazia, um `SNat n` tal que `n < Len (x : xs)`, retorna o tipo na posição `n` da lista de tipos `(x : xs)`.

```haskell
-- em HList.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

(!.) ::
  forall n x xs.
  (KnownNat n, n < Len (x : xs)) =>
  HList (x : xs) ->
  SNat n ->
  (x : xs) ! n
(y :. ys) !. sn = case natVal sn of
  0 -> y
  _ -> ys !. (natSing @(n - 1))
```

mas…

```
    • Could not deduce ‘xs ~ (x0 : xs0)’
      from the context: (KnownNat n, n < Len (x : xs))
        bound by the type signature for:
                   (!.) :: forall (n :: GHC.TypeNats.Nat) x (xs :: [*]).
                            (KnownNat n, n < Len (x : xs)) =>
                            HList (x : xs) -> SNat n -> (x : xs) ! n
        at src/Args/HList.hs:(24,1)-(29,14)
      Expected: HList (x0 : xs0)
        Actual: HList xs1
      ‘xs’ is a rigid type variable bound by
        the type signature for:
          (!.) :: forall (n :: GHC.TypeNats.Nat) x (xs :: [*]).
                   (KnownNat n, n < Len (x : xs)) =>
                   HList (x : xs) -> SNat n -> (x : xs) ! n
        at src/Args/HList.hs:(24,1)-(29,14)
    • In the first argument of ‘(!.)’, namely ‘ys’
      In the expression: ys !. (natSing @(n - 1))
      In a case alternative: _ -> ys !. (natSing @(n - 1))
    • Relevant bindings include
        (!.) :: HList (x : xs) -> SNat n -> (x : xs) ! n
          (bound at src/Args/HList.hs:30:11)
   |
32 |   _ -> ys !. (natSing @(n - 1))
   |
```

Aparentemente, como a função `(!.)` é chamada recursivamente, a cada chamada dela, os tipos envolvidos na assinatura mudam, mas o GHC precisa fixar os tipos com antecedência, ele não consegue equiparar os tipos que ele determinou com os tipos da assinatura que estão variando.

Depois de muito pesquisar e me inspirar [nesse post](https://hengchu.github.io/posts/2018-05-09-type-lists-and-type-classes.lhs.html), uma forma de contornar esse problema é definir uma *type class* e gerar instâncias para cada tipo *indutivamente*!

Crie um novo arquivo chamado `Pickable.hs` no módulo `Args`. Nele, vamos definir a nossa *type class* `Pickable`, que guardará a função `(!.)`

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}

type Pickable :: [Type] -> Nat -> Constraint
class (KnownNat n, n < Len xs) => Pickable xs n where
  (!.) :: HList xs -> SNat n -> xs ! n
```

`MultiParamTypeClasses` permite criar *type classes* com mais de um tipo paramétrico, e `UndecidableSuperClasses` para podermos aplicar a condição de que `n < Len xs` sempre.

Nosso caso base fica assim:

```haskell
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

instance (0 < Len (x : xs)) => Pickable (x : xs) 0 where
  (x :. _) !. _ = x
```

Agora precisamos habilitar `UndecidableInstances`, novamente por conta da restrição no tamanho. O `GADTs` foi habilitado por conta do *pattern matching*.

A instância do passo indutivo é meio feia…

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

instance
  {-# OVERLAPPABLE #-}
  ( KnownNat n,
    Pickable xs m,
    n ~ (m + 1),
    (xs ! m) ~ ((x : xs) ! n),
    n < Len (x : xs)
  ) =>
  Pickable (x : xs) n
  where
  (_ :. ys) !. _ = (!.) @xs @m ys (natSing @m)
```

Vamos por partes. Primeiro de tudo, perceba que teremos um conflito de instâncias quando n = 0. Desse modo, marcamos a instância indutiva com o *pragma* `OVERLAPPABLE` para indicar que na dúvida, o compilador pode sobrescrever essa instância e escolher uma mais específica.

Nas restrições, temos que garantir o `KnownNat` para que possamos usar o `natSing` como fizemos anteriormente. O `Pickable xs m` é nossa “hipótese indutiva”, e garante que existe uma instância de `Pickable` para `xs m`. Mas também temos que contar para o compilador que nosso `n` tem que ser equivalente a `m + 1`, e que o tipo em `xs ! m` é o mesmo que o tipo em `(x : xs) ! n`. Se não garantirmos isso ao compilador, nosso código será rejeitado. Finalmente, aplicamos a restrição do comprimento ao valor de `n`.

No âmbito da implementação, temos que usar o `TypeApplications` e aplicar os tipos `xs` e `m a (!.)` para que possamos usar a implementação da nossa hipótese indutiva, `Pickable xs m`, que o GHC entende que está bem estabelecida. E depois dessa especificação, passamos a cauda da lista e o antecessor de `n` como argumentos de `(!.)`.

Por fim, para que isso tudo dê certo agora, precisamos aplicar essa classe ao construtor `A` em `Op`:

```haskell
data Op xs a where
  A :: (KnownNat n, n < Len xs, Pickable xs n) => SNat n -> Op xs (xs ! n)
  ...
```

Parece que o GHC acatou nossa implementação agora! Agora podemos muito elegantemente (só não pode olhar debaixo do tapete!) implementar nosso `eval` para o argumento do usuário:

```haskell
eval :: AST xs a -> Args xs -> ST a
eval (A n) ls = Right $ ls !. n
```

Agora é o teste final. Vamos escrever uma pequena função que calcula nossa idade em um dado ano. Ela é a seguinte:

```haskell
-- | Calcula a minha idade em um dado ano
-- |
-- | Arg 0: Ano em que quero saber minha idade
-- | Arg 1: Ano em que eu nasci
-- | Retorna: minha idade no ano fornecido
ageIn :: AST '[Int, Int] Int
ageIn = SubInt (A $ natSing @0) (A $ natSing @1)
```

Vamos rodar ela no GHCi

```
ghci> eval ageIn (2054 :. 1998 :. Nil)
Right 56
```

Viva!! Estamos conseguindo colher *inputs* do usuário! E com segurança de tipos! De quebra, vamos implementar algumas funções de alta ordem.

## Map e Filter

Se em Haskell `map :: (a -> b) -> [a] -> [b]` e `a -> b` equivale, nos nossos termos, à `Op '[a] b`, então a implementação da operação de Map é a seguinte

```haskell
data Op xs a where
  ...
  Map :: Op '[a] b -> Op xs [a] -> Op xs [b]
```

`filter` é análogo

```haskell
Filter :: Op '[a] Bool -> Op xs [a] -> Op xs [a]
```

As implementações em `eval` são bem simples também

```haskell
eval (Map f xs) ls = do
  ys <- eval xs ls
  traverse (eval f . (:. Nil)) ys
eval (Filter p xs) ls = do
  ys <- eval xs ls
  let cond = eval p . (:. Nil)
  filterM cond ys
```

Consideravelmente mais simples do que as contrapartes do código anterior.

## Comentários finais

Incrivelmente, conseguimos realizar todos os objetivos que definimos no começo desse tutorial. O novo código é muito mais confiável, impedindo nos de escrever alguns absurdos.

Entretanto, poderia dizer que talvez ele seja muito restritivo. Isso pois, como cada operação carrega a lista dos seus argumentos, é muito difícil compor essas ASTs, pois as assinaturas têm que ser as mesmas, sempre. Isso é muito ruim, porque o coração da programação funcional é a composição.

Outro aspecto negativo é que ainda não conseguimos nos livrar de alguns erros de funções parciais, como `Sqrt` e `Div` . Se por acaso esquecermos de lidar com esses erros da maneira adequada, teremos erros sem tratamento voando pelo código a torto e a direito.

Uma proposta para lidar com essa situação é implementar tipos líquidos nesse código, e associar essas operações a predicados que garantam a segurança dos parâmetros passados.

Mas isso é assunto para um próximo tutorial…
