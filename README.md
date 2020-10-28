# iasc-stm-practica

Practica de laboratorio de STM 2019C1

### Times of lore

Este es el modelaje de un juego de un caballero que debe salvar a un reino en un tiempo de crisis. No nos importa mucho el modelo, solamente que tenemos un personaje que tiene una HP determinado, un inventario y una cantidad de oro determinado y puede interactuar con otros personajes NPC.

Dicho esto, solo modelaremos la parte de lo que son las transacciones de items, oro, compra/venta de items y no mucho más, para empezar modelemos al personaje.

~~~haskell
data Item = RedScroll
            | BlueScroll
            | Axe
            deriving (Eq, Ord, Show)

newtype Gold = Gold Int
    deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
    deriving (Eq, Ord, Show, Num)

type Inventory = [Item]
type Health = HitPoint
type Account = Gold

data Player = Player {
    account :: Account,
    health :: Health,
    inventory :: Inventory
}
~~~

Lo que tenemos ya modelado es el Item, el Oro, los puntos de salud, y esto es lo que conforma a nuestro personaje. Existen otros personajes en el reino y lo que nos interesa modelar en este caso es la transferencia de items u oro entre ellos. 

Antes de empezar a medida que se avancen en los puntos la estructuras de los datos que se menciona arriba puede cambiar, hay que pensar en el codigo actual como el primer paso antes de resolver vario de estos puntos incluso puede que no este soportado por STM.

#### Transferencia de Oro

El primer paso seria que tan solo tengamos una funcion que tome un monto a transferir y dos cuentas (Balance) y se mande ese oro de una cuenta a otra, se puede pensar una firma como la siguiente:

```haskell
transfer :: Gold -> Account -> Account -> STM ()
```

se puede hacer una version en donde se transfiera de una cuenta a otra pero a veces es tan solo mejor chequear tambien que si no se tienen suficientes fondos no deberia seguir con la operacion.

El codigo actual solo posee una implementacion base, habria que hacer en lo posible utilizando los conceptos de STM.

#### Dar un item

Ahora queremos que se pueda transferir un item de un inventario de un personaje a otro, y nos gustaria que esto se haga mediante la siguiente firma:

```haskell
giveItem :: Item -> Inventory -> Inventory -> STM ()
```

Para dar un item hay que primero sacarlo del primer inventario, incluso puede pasar que el item no este en el inventario de origen por lo que eso puede ser un resultado que nos de el item en cuestion o nada, para eso tenemos la funcion helper removeInv que ya nos dice esto

```haskell
removeInv :: Eq a => a -> [a] -> Maybe [a]
removeInv x xs =
    case span (/= x) xs of
      (_, [])                -> Nothing
      (prefix, (_ : suffix)) -> Just $ prefix ++ suffix
```

por lo que hay que ver si el item existe y en caso contrario sacarlo, y darselo al otro inventario. La idea es que tambien implemente STM y se pueda tener a los inventarios como un valor mutable.


### Vender un item

Ahora no queremos solo dar un item sin recibir nada a cambio, tambien queremos poder implementar la venta, para eso tendremos la funcion de sellItem, que va a tener la siguiente firma.

```haskell
sellItem :: Item -> Gold -> Player -> Player -> STM ()
```

Y que tendremos que verificar que se pueda vender el item, siempre y cuando

- El que compra el item tenga el oro suficiente
- El que venda tenga el item
- La transaccion tiene que verse como una sola, no puede pasar que el comprador tenga el oro y el vendedor no tenga el item, y el oro pase de una cuenta a otra

### Como instalarlo

Con [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Despues de eso hay que buildear o compilar lo que este en la carpeta library con 

`stack build`

y cuando se tengan que correr los tests solo hay que hacer

`stack test`