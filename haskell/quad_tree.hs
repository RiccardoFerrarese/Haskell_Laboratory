data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq,Show)

-- Esercizio 1
-- Si scriva una funzione buildNSimplify che dati 4 QuadTree costruisca un QuadTree la cui immagine codiﬁcata sia quella ottenuta dalle
-- 4 immagini corrispondenti ai 4 QuadTree messe nei quadranti superiore-sinistro, superiore-destro, inferiore-sinistro, inferiore-destro, rispettivamente.
-- (Attenzione che tutti sono e devono essere QuadTrees, non solo termini di tipo QT) --> se sono uguali sono termini

buildNSimplify :: Eq a => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify q1 q2 q3 q4 = if q1 == q2 && q1 == q3 && q1 == q4 then q1 else Q q1 q2 q3 q4

-- Esercizio 2
-- Si scriva una funzione simplify che dato un termine di tipo QT genera il QuadTree corrispondente.
simplify :: Eq a => QT a -> QT a
simplify qt = case qt of
                    C _           -> qt
                    Q q1 q2 q3 q4 -> buildNSimplify (simplify q1) (simplify q2) (simplify q3) (simplify q4)

-- Esercizio 3
-- Si scriva una funzione map che data una funzione f e un QuadTree q determina il QuadTree 
-- che codiﬁca l’immagine risultante dall’applicazione di f a tutti i pixel dell’immagine codiﬁcata da q

map_quad::Eq b => (a->b) -> QT a -> QT b
map_quad f (C x) = C (f x)
map_quad f (Q q1 q2 q3 q4) = buildNSimplify (map f q1) (map f q2) (map f q3) (map f q4)