import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)
import Data.List (intersperse)

data Naipe = Espadas | Copas | Paus | Ouros deriving (Show, Enum)
data Figura = As | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | Valete | Dama | Rei deriving (Show, Eq, Ord, Enum)
data Carta = Carta {figura :: Figura
                 , valor :: Int
                 , naipe :: Naipe
                 } deriving (Show)
newtype Dinheiro = Dinheiro Integer deriving (Eq, Ord)
instance Show Dinheiro where show (Dinheiro x) = show x

ganhou :: Dinheiro -> Dinheiro -> Dinheiro
Dinheiro x `ganhou` Dinheiro y = Dinheiro $ x + y

perdeu :: Dinheiro -> Dinheiro -> Dinheiro
Dinheiro x `perdeu` Dinheiro y = Dinheiro $ x - y

valorFigura :: Figura -> Int
valorFigura figura
  | figura >= Dez = 10
  | otherwise = fromEnum figura + 1

pontosDaMao :: [Carta] -> Int
pontosDaMao cartas = sum [valor carta | carta <- cartas]

temAs :: [Carta] -> Bool
temAs (carta:cartas)
  | figura carta == As = True
  | otherwise = temAs cartas
temAs _ = False

melhorValor :: [Carta] -> Int
melhorValor mao
  | temAs mao = if pontosDaMao' <= 11 then 
                    pontosDaMao' + 10 
                else 
                    pontosDaMao'
  | otherwise = pontosDaMao'
    where pontosDaMao' = pontosDaMao mao

displayDeMao :: String -> Bool -> [Carta] -> String
displayDeMao nome mostraAUltima mao = do
  let mostraMao = intersperse ", " [(show $ figura carta) ++ " de " ++ (show $ naipe carta) | carta <- mao]
      mostraCondicional = if mostraAUltima then mostraMao else init mostraMao ++ ["* de *"]
      mostraSoma = if mostraAUltima then " (" ++ show (melhorValor mao) ++ ")" else " (*)"
  nome ++ ": " ++ (concat mostraCondicional) ++ mostraSoma

hit :: [Carta] -> [Carta] -> ([Carta], [Carta])
hit mao (carta:cartas) = (carta:mao, cartas)

leHitStay :: [Carta] -> [Carta] -> IO ([Carta], [Carta])
leHitStay maoJogador deck = do
  putStrLn "(h)it ou (s)tay: "
  hitOuStay <- getLine
  case hitOuStay of
    "" -> leHitStay maoJogador deck
    "h" -> pure (hit maoJogador deck)
    "s" -> pure (maoJogador, deck)
    otherwise -> leHitStay maoJogador deck


dealerHits :: [Carta] -> [Carta] -> ([Carta], [Carta])
dealerHits maoDealer deck
  | melhorValor maoDealer < 17 = let (maoD, d) = hit maoDealer deck in dealerHits maoD d
  | otherwise = (maoDealer, deck)

jogadorHistStay :: [Carta] -> [Carta] -> IO ([Carta], [Carta], Int)
jogadorHistStay maoJogador deck = do
  putStrLn $ displayDeMao "Jogador" True maoJogador
  if pontosJogador == 21 then
    pure (maoJogador, deck, 21)
  else if pontosJogador > 21 then
    pure (maoJogador, deck, pontosJogador)
  else do
    (maoJ, d) <- leHitStay maoJogador deck
    if length maoJ == length maoJogador then
      pure (maoJogador, deck, pontosJogador)
    else
      jogadorHistStay maoJ d
  where pontosJogador = melhorValor maoJogador

validaAposta :: Dinheiro -> Dinheiro -> IO Dinheiro
validaAposta aposta bancaJogador
  | aposta >= Dinheiro 0 && aposta <= bancaJogador = return aposta
  | otherwise = do
      putStrLn $ "Sua aposta não foi entre 1 e " ++ show bancaJogador ++ "!"
      leApostaValida bancaJogador

leApostaValida :: Dinheiro -> IO Dinheiro
leApostaValida bancaJogador = do
  putStrLn "Por favor, insira sua aposta: "
  aposta <- fmap Dinheiro . readMaybe <$> getLine
  case aposta of
    Nothing -> leApostaValida bancaJogador
    Just n -> validaAposta n bancaJogador

rodadaDeAposta :: Dinheiro -> IO Dinheiro
rodadaDeAposta bancaJogador = do
  putStrLn "\n"

  if bancaJogador == Dinheiro 0 then do
    putStrLn $ "Você quebrou a banca!"
    pure (Dinheiro 0)
  else do
    putStrLn $ "você tem R$" ++ show bancaJogador


    let cartas = [Carta f (valorFigura f) n | n <- [Espadas ..], f <- [As ..]]


    let cartasEmbaralhadas = shuffleM cartas :: IO [Carta]


    embaralhadas <- cartasEmbaralhadas
    let maoJogadorEBaralho = splitAt 2 embaralhadas
    let maoDealerEBaralho = splitAt 2 (snd maoJogadorEBaralho)
    let deck = snd maoDealerEBaralho

    let maoDealer = fst maoDealerEBaralho
    let maoJogador = fst maoJogadorEBaralho


    apostaFeita <- leApostaValida bancaJogador
    putStrLn $ "Sua aposta: $" ++ show apostaFeita


    putStrLn $ displayDeMao "Dealer" False maoDealer


    (maoFinalJogador, finalDeck, pontosJogador) <- jogadorHistStay maoJogador deck

    if pontosJogador == 21 then do
      putStrLn "Blackjack! Jogador vence!"
      rodadaDeAposta $ bancaJogador `ganhou` apostaFeita
    else if pontosJogador > 21 then do
      putStrLn "Bust! Jogador perde!"
      rodadaDeAposta $ bancaJogador `perdeu` apostaFeita
    else do
      putStrLn $ displayDeMao "Jogador" True maoFinalJogador


      let maoFinalJDealer = fst $ dealerHits maoDealer finalDeck
      putStrLn $ displayDeMao "Dealer" True maoFinalJDealer

      if melhorValor maoFinalJDealer > 21 then do
        putStrLn "Dealer busted! Jogador vence!"
        rodadaDeAposta $ bancaJogador `ganhou` apostaFeita
      else if melhorValor maoFinalJDealer == 21 then do
        putStrLn "Dealer fez blackjack! Dealer vence!"
        rodadaDeAposta $ bancaJogador `perdeu` apostaFeita
      else if melhorValor maoFinalJDealer > pontosJogador then do
        putStrLn "Dealer tem mão melhor! Dealer vence!"
        rodadaDeAposta $ bancaJogador `perdeu` apostaFeita
      else do 
        putStrLn "Jogador tem mão melhor! Player vence!"
        rodadaDeAposta $ bancaJogador `ganhou` apostaFeita

main :: IO()
main = do
  rodadaDeAposta $ Dinheiro 100
  putStrLn "Fim de jogo!"