#pragma once

#include <memory>
#include <unordered_map>
#include <variant>
#include <string>
#include <vector>
#include <optional>

/*!
 * @brief Immutable context container for CodeUChain data flow
*/

namespace codeuchain {

using DataValue = std::variant<
    std::monostate,    // null/empty
    int,
    double,
    bool,
    std::string,
    std::vector<std::string>
>;

class Context {
public:
    // Create empty context
    Context();

    // Create context with initial data
    explicit Context(std::unordered_map<std::string, DataValue> data);

    // Copy constructor (immutable)
    Context(const Context& other);

    // Move constructor
    Context(Context&& other) noexcept;

    // Assignment operators
    Context& operator=(const Context& other);
    Context& operator=(Context&& other) noexcept;

    // Insert new data (returns new context)
    [[nodiscard]] Context insert(std::string key, DataValue value) const;

    // Get data by key
    [[nodiscard]] std::optional<DataValue> get(const std::string& key) const;

    // Update existing data (returns new context)
    [[nodiscard]] Context update(std::string key, DataValue value) const;

    // Check if key exists
    [[nodiscard]] bool has(const std::string& key) const;

    // Get all keys
    [[nodiscard]] std::vector<std::string> keys() const;

    // Remove data (returns new context)
    [[nodiscard]] Context remove(const std::string& key) const;

    // Clear all data (returns new context)
    [[nodiscard]] Context clear() const;

    // Get data size
    [[nodiscard]] size_t size() const;

    // Check if empty
    [[nodiscard]] bool empty() const;

    // ===== PERFORMANCE OPTIMIZATION METHODS =====
    // For high-frequency mutations within a single link
    // WARNING: Use only when performance is critical and you understand the implications
    // These methods modify the context in-place, breaking immutability guarantees

    // Mutable insert (modifies this context) - USE SPARINGLY
    void insert_mut(std::string key, DataValue value);

    // Mutable update (modifies this context) - USE SPARINGLY
    void update_mut(std::string key, DataValue value);

    // Mutable remove (modifies this context) - USE SPARINGLY
    void remove_mut(const std::string& key);

    // Mutable clear (modifies this context) - USE SPARINGLY
    void clear_mut();

private:
    std::shared_ptr<std::unordered_map<std::string, DataValue>> data_;
};

} // namespace codeuchain
