let selectedDish = null;

        // Add click event to table rows
        document.querySelectorAll("#menuTable tbody tr").forEach(row => {
            row.addEventListener("click", () => {
                // Remove "selected" class from all rows
                document.querySelectorAll("#menuTable tbody tr").forEach(r => r.classList.remove("selected"));

                // Add "selected" class to the clicked row
                row.classList.add("selected");

                // Store selected dish and price
                const dishName = row.cells[0].innerText;
                const price = row.cells[1].innerText;

                selectedDish = { name: dishName, price: price };
            });
        });

        // Add click event to "Place Order" button
        document.getElementById("placeOrder").addEventListener("click", () => {
            const orderInfo = document.getElementById("orderInfo");

            if (selectedDish) {
                // Display selected dish and price
                orderInfo.innerHTML = `You selected: <strong>${selectedDish.name}</strong> costing <strong>$${selectedDish.price}</strong>.`;
            } else {
                orderInfo.innerHTML = `<span style="color: red;">Please select a dish before placing an order!</span>`;
            }
        });