import React, { useState, useMemo } from "react";
import "./Menu.css";
import {menuItems, MenuItem} from "./Data.ts"

const Menu: React.FC<{}> = () => {
  const [searchTerm, setSearchTerm] = useState("");
  const [sortKey, setSortKey] = useState<keyof MenuItem>("price");
  const [sortOrder, setSortOrder] = useState<"asc" | "desc">("asc");
  const [email, setEmail] = useState("");
  const [message, setMessage] = useState("Empty: ");
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
    const lowerCaseSearchTerm = searchTerm.toLowerCase();
    return menuItems.filter((item) => {
      const lowerCaseName = item.name.toLowerCase();
      const lowerCaseDescription = item.description.toLowerCase();
      return (
        lowerCaseName.includes(lowerCaseSearchTerm) ||
        lowerCaseDescription.includes(lowerCaseSearchTerm)
      );
    });
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

  const handleSubmit = (e: any) => {
    e.preventDefault();

    // Tworzenie wiadomości
    let emailBody = "Wybrane pieseły:\n";
    Array.from(selectedItems).map((index) => {
      emailBody += `${sortedItems[index].name} - ${sortedItems[index].price} zł \r\n`;
    });
    setMessage(emailBody);
    // Tutaj możesz dodać logikę wysyłania maila, np. za pomocą fetch lub axios
    console.log("Wysłano ", emailBody, " do:", email);
  };

  return (
    <div>
      <input
        type="text"
        placeholder="Choose your dog"
        value={searchTerm}
        onChange={(e) => setSearchTerm(e.target.value)}
      />

      <table>
        <thead>
          <tr>
            <th></th>
            <th>Nazwa</th>
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
        <ul>
          {Array.from(selectedItems).map((index) => (
            <li key={index}>{sortedItems[index].name}</li>
          ))}
        </ul>
      </div>

      <form onSubmit={handleSubmit}>
        <label htmlFor="email">Email for order confirmation and payment info: </label>
        <input
          type="email"
          id="email"
          value={email}
          onChange={(e) => setEmail(e.target.value)}
          required
        />
        <button type="submit">Order doggie</button>
        {message && <p>{message}</p>}
      </form>
    </div>
  );
};

export default Menu;
