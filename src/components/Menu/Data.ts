interface MenuItem {
  id: number;
  name: string;
  description: string;
  price: number;
  image: string;
}

const menuItems: MenuItem[] = [
  {
    id: 1,
    name: "Pizza Margherita",
    description: "Klasyczna pizza z sosem pomidorowym, mozzarellą i bazylią.",
    price: 250,
    image: "https://example.com/pizza-margherita.jpg",
  },
  {
    id: 2,
    name: "Spaghetti Carbonara",
    description:
      "Makaron spaghetti z sosem śmietanowym, jajkami, boczkiem i parmezanem.",
    price: 30,
    image: "https://example.com/spaghetti-carbonara.jpg",
  },
];

export { menuItems };
export type { MenuItem };
