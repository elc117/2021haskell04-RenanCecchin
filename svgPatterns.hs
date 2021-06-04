import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)

type Diamond   = (Point, Point, Point, Point)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) só com tons de verde "hard-coded" 
-- (pode ser melhorado substituindo os valores literais por parâmetros)
-- Além disso, o que acontecerá se n for muito grande ou negativo?
--greenPalette :: Int -> [(Int,Int,Int)]
--greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]

yellowPalette :: Int -> [(Int, Int, Int)]
yellowPalette n = [(255, 223, 0)]

greenPalette :: Int -> [(Int, Int, Int)]
greenPalette n = [(0, 156 + n, 59 + n)]

bluePalette :: Int -> [(Int, Int, Int)]
bluePalette n = [(0, 39 + n, 118 + n)]

-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 0.0), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 10

genGreenRect :: (Float, Float) -> Rect
genGreenRect (x,y) = ((0, 0), x, y)

genYellowDiamond :: (Float, Float) -> Diamond
genYellowDiamond (x,y) = ((0 + (x/10), (y/2)), ((x/2),y - (y/10)), ((x - (x/10)), (y/2)), ((x/2),0 + (y/10)))

genBlueCircle :: (Float, Float) -> Circle
genBlueCircle (x,y) = (((x/2), (y/2)), (x/9))
-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style


-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

svgDiamond :: Diamond -> String -> String
svgDiamond ((a,b), (c,d), (e,f), (g,h)) style =
  printf "<polygon points='%.3f,%.3f %.3f,%.3f %.3f,%.3f %.3f,%.3f' style='%s' />\n" a b c d e f g h style

svgCircle :: Circle -> String -> String
svgCircle ((x,y), r) style =
  printf "<circle cx='%.3f' cy='%.3f' r='%.3f' style='%s' />\n" x y r style

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  --writeFile "rects.svg" $ svgstrs
  --where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
  --      svgfigs = svgElements svgRect rects (map svgStyle palette)
  --      rects = genRectsInLine nrects
  --      palette = rgbPalette nrects
  --      nrects = 10
  --      (w,h) = (1500,500) -- width,height da imagem SVG
-----------------------------------------------------------------------------
-- Novo
-----------------------------------------------------------------------------
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgrects ++ svgdiamonds ++ svgcircles ++ svgEnd
        svgrects = svgElements svgRect rects (map svgStyle rectpalette)
        svgdiamonds = svgElements svgDiamond diamonds (map svgStyle diamondpalette)
        svgcircles = svgElements svgCircle circles (map svgStyle bluepalette)
        rects = [genGreenRect (w,h)]
        diamonds = [genYellowDiamond (w,h)]
        circles = [genBlueCircle (w,h)]
        rectpalette = greenPalette gap
        diamondpalette = yellowPalette gap
        bluepalette = bluePalette gap
        gap = 0
        (w,h) = (1200,500) -- width,height da imagem SVG



