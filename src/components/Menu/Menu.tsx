import React, { useState, useMemo } from "react";
import "./Menu.css";

interface MenuItem {
  name: string;
  description: string;
  price: number;
  image: string;
}

const menuItems: MenuItem[] = [
  {
    name: "Pizza Margherita",
    description: "Klasyczna pizza z sosem pomidorowym, mozzarellą i bazylią.",
    price: 250,
    image: "https://example.com/pizza-margherita.jpg",
  },
  {
    name: "Spaghetti Carbonara",
    description:
      "Makaron spaghetti z sosem śmietanowym, jajkami, boczkiem i parmezanem.",
    price: 30,
    image: "https://example.com/spaghetti-carbonara.jpg",
  },
];

const Menu: React.FC<{}> = () => {
  const [searchTerm, setSearchTerm] = useState("");
  const [sortKey, setSortKey] = useState<keyof MenuItem>("price");
  const [sortOrder, setSortOrder] = useState<"asc" | "desc">("asc");
  const [selectedItems, setSelectedItems] = useState<Set<number>>(new Set());

  const handleCheckboxChange = (index: number) => {
    const newSelectedItems = new Set(selectedItems);
    if (newSelectedItems.has(index)) {
      newSelectedItems.delete(index);
    } else {
      newSelectedItems.add(index);
    }
    setSelectedItems(newSelectedItems);
  };

  const filteredItems = useMemo(() => {
    return menuItems.filter((item) =>
      item.name.toLowerCase().includes(searchTerm.toLowerCase())
    );
  }, [menuItems, searchTerm]);

  const sortedItems = useMemo(() => {
    const sorted = [...filteredItems].sort((a, b) => {
      const aValue = a[sortKey];
      const bValue = b[sortKey];

      if (typeof aValue === "number" && typeof bValue === "number") {
        return sortOrder === "asc" ? aValue - bValue : bValue - aValue;
      } else {
        const aString = String(aValue);
        const bString = String(bValue);
        return sortOrder === "asc"
          ? aString.localeCompare(bString)
          : bString.localeCompare(aString);
      }
    });
    return sorted;
  }, [filteredItems, sortKey, sortOrder]);

  const handleSort = (key: keyof MenuItem) => {
    if (key === sortKey) {
      setSortOrder(sortOrder === "asc" ? "desc" : "asc");
    } else {
      setSortKey(key);
      setSortOrder("asc");
    }
  };

  return (
    <div>
      <input
        type="text"
        placeholder="Szukaj po nazwie..."
        value={searchTerm}
        onChange={(e) => setSearchTerm(e.target.value)}
      />

      <table>
        <thead>
          <tr>
            <th onClick={() => handleSort("name")}>
              Nazwa {sortKey === "name" && (sortOrder === "asc" ? "▲" : "▼")}{" "}
            </th>
            <th>Opis</th>
            <th onClick={() => handleSort("price")}>
              Cena {sortKey === "price" && (sortOrder === "asc" ? "▲" : "▼")}{" "}
            </th>
            <th>Zdjęcie</th>
          </tr>
        </thead>
        <tbody>
          {sortedItems.map((item, index) => (
            <tr key={index}>
              <td>
                <input
                  type="checkbox"
                  checked={selectedItems.has(index)}
                  onChange={() => handleCheckboxChange(index)}
                />
              </td>
              <td>{item.name}</td>
              <td>{item.description}</td>
              <td>{item.price} zł</td>
              <td>
                <img
                  src={item.image}
                  alt={item.name}
                  style={{ maxWidth: "100px" }}
                />
              </td>
            </tr>
          ))}
        </tbody>
      </table>

      {/* Wyświetlamy listę zaznaczonych elementów (opcjonalne) */}
      <div>
        Wybrane elementy:
        <ul>
          {Array.from(selectedItems).map((index) => (
            <li key={index}>{sortedItems[index].name}</li>
          ))}
        </ul>
      </div>
    </div>
  );
};

export default Menu;
