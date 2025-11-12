+++
date = '2024-03-25T00:00:00-03:00'
draft = false
title = 'Representando programas com ADTs em Haskell'
tags = ['Haskell', 'Interpretadores', 'Tipos de dados algébricos']
+++

*Disclaimer: este artigo e o código usado nele foram fortemente baseados no
[repositório do Mateus](https://github.com/mcf1110/hotgp). O Mateus é um aluno do doutorado da UFABC
e o repositório em questão é um algoritmo de programação genética (que resumidamente
é um programa que descobre programas com base em um conjunto de especificações) chamado
HOTGP, desenvolvido por ele. Recomendo fortemente visitar o repositório e dar uma lida
no [artigo](https://arxiv.org/pdf/2304.03200)!*

*Por brevidade, algumas funções não foram incluídas no texto deste artigo.
Para o código na íntegra, acesse o [repositório do Github](https://github.com/pedro-moren0/dgt-tutorial1).*

---

Haskell é uma linguagem muito moderna, que se vale de ferramentas matemáticas muito interessantes e poderosas,
a ponto de influenciar, direta ou indiretamente, outras linguagens de programação. Dentre essas ferramentas,
acredito que a mais poderosa seja a força que os tipos têm na linguagem.

Como estudo de caso dessa força, neste tutorial vamos representar programas com tipos na forma de Árvore
Sintática Abstrata (*Abstract Syntax Tree*, ou AST, como será chamado daqui em diante, além de implementar a
execução dos mesmos.

## O que é uma AST?

Existem inúmeras maneiras de se representar programas de computador.
Uma AST nada mais é do que uma dessas maneiras.
Podemos encarar o programa como uma árvore,
onde seus nós são operações (isto é, funções),
e os ramos que saem desses nós podem ser tanto outras funções quanto folhas que guardam literais ou parâmetros
fornecidos.
Por exemplo, se temos uma expressão como `if name == "Alice" then "Hello, Alice!" else "Hello, world!"`,
sua AST ficaria dessa forma:

{{< figure src="https://miro.medium.com/v2/resize:fit:720/format:webp/1*dl_8NPStN0BoAbgVq4MP2Q.png" title="Uma possível AST para a expressão acima. Os nós em cinza indicam valores constantes e valores em amarelo indicam argumentos fornecidos ao programa. Note que, a título de exemplo, consideramos as Strings como tipos primitivos, mas elas ainda poderiam ser consideradas como listas de caracteres." >}}

Se estamos lidando com uma linguagem de programação imperativa, enxergar o programa dessa maneira pode não ser trivial. Mas no caso de um programa funcional, onde pensamos em programar como uma série de transformações ao invés de uma série de instruções, a representação de AST cai como uma luva.

## Mãos à obra!

Finalmente, vamos começar a desenvolver a nossa linguagem! Iremos usar o GHCup com Stack e suporte para o Haskell Language Server (HLS) para desenvolver esse tutorial. Inicie um projeto novo na sua pasta de escolha com `stack new NOME_DO_SEU_PROJETO simple-hpack` para inicializar o projeto (inicializar o projeto com o template do hpack não é necessário, mas para mim ele integra muito bem com o HLS e não precisamos ficar editando o .cabal). Em `src` crie a pasta `Grammar` e dentro dela crie o arquivo `Core.hs`. Nesse arquivo iremos definir a base da nossa gramática.

### Core.hs

Primeiro vamos definir os tipos que a gramática irá suportar:

```haskell
-- Grammar/Core.hs

data Tipo
  = IntT
  | FloatT
  | CharT
  | BoolT
  | PolyT Int
  | PairT Tipo Tipo
  | ListT Tipo
  | LambdaT FunctionT
  deriving (Show, Eq)
```

São tipos muito básicos: inteiros, números de ponto flutuante, caracteres e booleanos. Também temos listas e pares, parametrizados recursivamente pelos tipos. Dois tipos que merecem explicação são `PolyT` e `LambdaT` . `PolyT` é o tipo polimórfico. Por exemplo, qual é o tipo de retorno de uma expressão `if` ?Esse valor varia, então devemos contabilizar a possibilidade de diversos tipos. O `Int` associado é um identificador para indicar que temos dois tipos paramétricos mas que pode ser diferentes, por exemplo um `a0` e um `a1` . Já o `LambdaT` é o tipo das funções lambda (sim, nossa linguagem terá suporte para funções de alta ordem, mas você ainda não sabe disso). `FunctionT` é a assinatura da função, composta por uma lista de tipos de entrada e um tipo de saída. Adicione essas definições a seguir.

```haskell
data FunctionT = FunctionT {_argT :: ArgT, _outT :: OutT} deriving (Show, Eq)
type ArgT = [Tipo]
type OutT = Tipo
```

Definidos os tipos, vamos definir qual será a cara de um AST na nossa linguagem:

```haskell
data AST = Leaf Terminal | Node Op [AST] deriving (Show)
data TAST = TAST {_ast :: AST, _ftype :: FunctionT} deriving (Show)
```

Até aqui nada de especial também: nossa AST é uma folha que contém um terminal, ou ela é um nó que tem uma operação associada e uma sequência de ramos que também são ASTs. Também temos uma *Typed AST* (TAST), que nada mais é que uma AST munida de uma assinatura de função. Ela será útil no contexto das funções de alta ordem.

Um terminal é definido da seguinte forma: ou ele é um literal, ou ele é um argumento, onde o inteiro associado ao argumento é o índice referente a uma lista de argumentos fornecida ao programa.

```haskell
data Terminal = Literal Lit | Arg Int deriving (Show)

data Lit
  = IntL Int
  | FloatL Float
  | CharL Char
  | BoolL Bool
  | PairL Lit Lit
  | ListL Tipo [Lit]
  | LambdaL TAST
  deriving (Show)
```

Mudando nosso foco para os literais, ele nada mais são que envelopes para os próprios tipos do Haskell, com exceção de `PairL` e `ListL` , que são definidos recursivamente mas ainda são envelopes em algum nível, e `LambdaL` que guarda uma TAST, ou seja, a lógica de execução dessa função lambda munida da sua assinatura.

As operações suportadas serão as seguintes:

```haskell
data Op
  = AddInt
  | SubInt
  | MultInt
  | DivInt
  | ModInt
  | LTEInt
  | EqInt
  | AddFloat
  | SubFloat
  | MultFloat
  | DivFloat
  | Sqrt
  | EqChar
  | IsLetter
  | IsDigit
  | ToUpper
  | ToLower
  | And
  | Or
  | Not
  | If
  | ToPair
  | Fst
  | Snd
  | Len
  | Cons
  | Head
  | Tail
  | Map
  | Filter
  deriving (Show)
```
   
Isso conclui nosso trabalho em `Core.hs`.

### Eval.hs

Também em `Grammar` , crie `Eval.hs` . Aqui implementaremos a lógica de avaliação de uma AST.

Para representar operações falíveis, iremos usar o `Either` para guardar um *stack trace* em caso de falha, que nada mais é que uma String na qual iremos concatenar as mensagens de erro. Eis um trecho de `eval`

```haskell
type StackTrace = String

eval :: Op -> [Lit] -> Either StackTrace Lit
eval AddInt [IntL x1, IntL x2] = Right $ IntL (x1 + x2)
eval SubInt [IntL x1, IntL x2] = Right $ IntL (x1 - x2)
eval MultInt [IntL x1, IntL x2] = Right $ IntL (x1 * x2)
eval op@ModInt args@[IntL x1, IntL x2]
  | x2 == 0 = Left $ makeFullStacktrace op args "\nDivision by zero!"
  | otherwise = Right $ IntL (x1 `mod` x2)
```

A ideia aqui é muito básica: fazemos um *pattern matching* com o nome da operação e os literais dentro da lista. Se não há possibilidade de falha, extraímos os valores dos envelopes de `Lit`, realizamos as operações usando as próprias funções do Haskell, colocamos o resultado de volta no envelope adequado e passamos ele para `Right`. Caso haja possibilidade de falha, criamos um caso separado usando os *guards*, criamos o *stack trace* e passamos ele para `Left`. Esse padrão é muito frequente em `eval`.

Alguns casos de eval não seguem esse padrão acima, e merecem um breve comentário. São eles `Map` e `Filter`.

```haskell
eval op@Map args@[LambdaL tast, ListL _ xs] = case stOrLits of
  Right lits -> Right $ ListL ((_outT . _ftype) tast) lits
  Left st -> Left $ makeFullStacktrace op args st
  where
    stOrLits :: Either StackTrace [Lit]
    stOrLits = sequenceA (xs >>= \x -> pure $ evalAST [x] (_ast tast))
eval op@Filter args@[LambdaL tast, ListL t xs] = case stOrLits of
  Right lits -> Right $ ListL t lits
  Left st -> Left $ makeFullStacktrace op args st
  where
    stOrLits :: Either StackTrace [Lit]
    stOrLits = sequenceA $ do
      x <- xs
      let stOrLit = evalAST (pure x) (_ast tast)
      let res = [] :: [Either StackTrace Lit]
      case stOrLit of
        Right (BoolL b) -> if b then Right x : res else res
        Right _ -> res
        Left st -> Left (makeFullStacktrace op args st) : res
```

Para `Map`, precisamos avaliar a AST da lambda para cada elemento da lista fornecida. Depois usamos `sequenceA`, que pode retornar um `Right [Lit]` se todas as operações forem bem sucedidas, ou um `Left StackTrace` caso alguma operação tenha falhado (abusando da linguagem). Semelhante é `Filter`, mas que irá retornar uma lista de `BoolL` da aplicação da AST à lista, e basta incluirmos o x que gerou o tal `BoolL` se seu valor for `True`.

Finalmente em `eval` temos um caso fallback para lidar com erros

```haskell
eval op args
  | null args = Left $ makeFullStacktrace op args "\nNo arguments provided!"
  | argsLen < ariOp op = Left $ makeFullStacktrace op args "\nToo little arguments provided!"
  | argsLen > ariOp op = Left $ makeFullStacktrace op args "\nToo many arguments provided!"
  | otherwise = Left $ makeFullStacktrace op args ""
  where
    argsLen = length args
```

E agora lidaremos com `evalAST` , que é muito mais simples e sucinta que `eval`:

```haskell
evalAST :: [Lit] -> AST -> Either StackTrace Lit
evalAST args (Leaf (Arg argNum)) =
  if argNum < argsLen
    then Right $ args !! argNum
    else
      Left $
        "\nInvalid access to provided arguments.\nYou tried to access the "
          <> show (argNum + 1)
          <> ordAbrv (argNum + 1)
          <> " argument when "
          <> show argsLen
          <> " arguments were provided"
  where
    argsLen = length args

    ordAbrv :: Int -> String
    ordAbrv 1 = "st"
    ordAbrv 2 = "nd"
    ordAbrv 3 = "rd"
    ordAbrv _ = "th"
evalAST _ (Leaf (Literal lit)) = Right lit
evalAST args (Node op asts) = case traverse (evalAST args) asts of
  Right lits -> eval op lits
  Left st -> Left $ putOpInStacktrace op st
```

Se AST é uma folha, ela pode ser um literal ou um argumento fornecido. Caso seja um literal, a avaliação dessa AST é o próprio valor do literal, e caso seja um argumento, é o valor do argumento caso ele exista. Esses são os casos base.

Já o caso recursivo segue este raciocínio: aplicamos recursivamente o `evalAST` aos ramos do nó e coletamos o resultado em um `Either`. Se a aplicação recursiva foi bem sucedida, aplicamos a lista de literais encontrada à operação no nó raiz.

## *Test Drive*

Vamos botar nosso experimento para rodar! Crie um arquivo `Examples.hs` em `src` e abra-o no `ghci`.

```haskell
-- | Uma expressao
expr1 :: AST
expr1 =
  If
    -< [ IsDigit
           -< [Leaf $ Literal (CharL 'c')],
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

-- | Uma expressao com erro
exprErr1 :: AST
exprErr1 =
  If
    -< [ IsDigit
           -< [Leaf $ Literal (IntL 1)],
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

-- | Outra expressao com erro
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
                Leaf $ Literal (IntL 7),
                Leaf $ Literal (IntL 7),
                Leaf $ Literal (IntL 7),
                Leaf $ Literal (IntL 7)
              ],
         ToUpper
           -< [Leaf $ Literal (CharL 'c')]
       ]

-- | Calcula a distancia euclidiana de dois pontos no plano
-- |
-- | Arg 0: Primeiro ponto
-- | Arg 1: Segundo ponto
-- | Retorna: a distancia entre os dois pontos fornecidos
dist :: AST
dist =
  Sqrt
    -< [ AddFloat
           -< [ MultFloat
                  -< [ SubFloat
                         -< [ Fst -< [Leaf $ Arg 1],
                              Fst -< [Leaf $ Arg 0]
                            ],
                       SubFloat
                         -< [ Fst -< [Leaf $ Arg 1],
                              Fst -< [Leaf $ Arg 0]
                            ]
                     ],
                MultFloat
                  -< [ SubFloat
                         -< [ Snd -< [Leaf $ Arg 1],
                              Snd -< [Leaf $ Arg 0]
                            ],
                       SubFloat
                         -< [ Snd -< [Leaf $ Arg 1],
                              Snd -< [Leaf $ Arg 0]
                            ]
                     ]
              ]
       ]

-- | Eleva os elementos de uma lista de inteiros ao quadrado
-- | e filtra os elementos pares
-- |
-- | Arg 0: uma lista de inteiros
-- | Retorna: uma lista com todos os quadrados pares
squareAndFilterEvens :: AST
squareAndFilterEvens =
  Filter
    -< [ (\.)
           ( EqInt
               -< [ ModInt -< [Leaf $ Arg 0, Leaf $ Literal (IntL 2)],
                    Leaf $ Literal (IntL 0)
                  ]
           )
           ([IntT] --> BoolT),
         Map
           -< [ (\.)
                  (MultInt -< [Leaf $ Arg 0, Leaf $ Arg 0])
                  ([IntT] --> IntT),
                Leaf $ Arg 0
              ]
       ]
```

---

```haskell
-- Eval.hs

printLitST :: Either StackTrace Lit -> IO ()
printLitST (Left st) = putStrLn st
printLitST (Right lit) = print lit
```

---

```
ghci> :l Examples
...
ghci> printLitST $ evalAST [] expr1
CharL 'C'
```

Funciona! Naturalmente não importa o valor que passemos como lista nesse caso, pois uma expressão não depende de argumentos.

Vamos tentar avaliar `dist`:

```
ghci> printLitST $ evalAST [PairL (FloatL 0.0) (FloatL 0.0), PairL (FloatL 3.0) (FloatL 4.0)] dist 
FloatL 5.0
```

Também funciona! E é um resultado correto: a hipotenusa de um triângulo pitagórico tem 5 unidades de distância.

Vamos ver `squareAndFilterEvens`:

```
ghci> printLitST $ evalAST [ListL IntT [IntL x | x <- [1..6]]] squareAndFilterEvens  
ListL IntT [IntL 4,IntL 16,IntL 36]
```

E os *stack traces*:

```
ghci> printLitST $ evalAST [] exprErr1 

Error: args [IntL 1]
At: IsDigit
At: If
ghci> printLitST $ evalAST [] exprErr2 

Too many arguments provided!
Error: args [IntL 17,IntL 7,IntL 7,IntL 7,IntL 7]
At: SubInt
At: If
```

## Considerações finais

Nosso código funciona! Conseguimos representar programas com nossa definição de AST feita em ADTs, e até conseguimos incluir funções de alta ordem! Mas… o código não está muito legal.

1. Os tipos e os literais vivem em mundos separados, e precisamos de funções para juntar os dois. Se por acaso esquecermos de atualizar a função que faz esse mapeamento, ou a atualizarmos de maneira errada, nada irá impedir desse erro acontecer.
2. Na definição de `Op`, tínhamos essa operação: `DivInt` . Mas ela nunca foi implementada. O *fallback* de erro ao final de `eval` ocultou a existência desse valor, e o GHC não pode deduzir que a implementação dele estava ausente.
3. Como visto em `exprErr1` e `exprErr2` , ainda podemos representar estados impossíveis, o que nos leva a ter que implementar error handlings por todas as partes do código.

Isso nos faz pensar: será que existe maneira melhor de realizar essa implementação?
