#pragma once

#include "context.hpp"
#include <memory>
#include <unordered_map>
#include <variant>
#include <string>
#include <vector>
#include <optional>
#include <type_traits>

/*!
 * @brief Typed Context extensions for CodeUChain
 *
 * Implements opt-in generics that provide static type safety while maintaining
 * runtime flexibility. Extends the base Context with typed operations.
 */

namespace codeuchain {

// Forward declaration of base Context
class Context;

// ===== TYPED DATA VALUE =====
// Extends DataValue with typed variants for compile-time type safety
template<typename T>
struct TypedDataValue {
    T value;

    TypedDataValue(const T& val) : value(val) {}

    // Allow implicit conversion to base DataValue for runtime flexibility
    operator DataValue() const {
        if constexpr (std::is_same_v<T, int>) {
            return DataValue(value);
        } else if constexpr (std::is_same_v<T, double>) {
            return DataValue(value);
        } else if constexpr (std::is_same_v<T, bool>) {
            return DataValue(value);
        } else if constexpr (std::is_same_v<T, std::string>) {
            return DataValue(value);
        } else if constexpr (std::is_same_v<T, std::vector<std::string>>) {
            return DataValue(value);
        } else {
            // For unsupported types, store as string representation
            return DataValue(std::to_string(value));
        }
    }
};

// ===== TYPED CONTEXT =====
// Generic context that maintains type information at compile time
template<typename T = std::monostate>
class TypedContext {
public:
    // Default constructor
    TypedContext() : context_(std::make_shared<Context>()) {}

    // Constructor from base Context
    explicit TypedContext(const Context& ctx) : context_(std::make_shared<Context>(ctx)) {}

    // Constructor from typed data
    explicit TypedContext(std::unordered_map<std::string, DataValue> data)
        : context_(std::make_shared<Context>(std::move(data))) {}

    // Copy constructor
    TypedContext(const TypedContext& other) : context_(other.context_) {}

    // Move constructor
    TypedContext(TypedContext&& other) noexcept : context_(std::move(other.context_)) {}

    // Assignment operators
    TypedContext& operator=(const TypedContext& other) {
        if (this != &other) {
            context_ = other.context_;
        }
        return *this;
    }

    TypedContext& operator=(TypedContext&& other) noexcept {
        context_ = std::move(other.context_);
        return *this;
    }

    // ===== TYPED OPERATIONS =====

    // Typed insert - preserves type information
    template<typename U>
    [[nodiscard]] TypedContext<T> insert(const std::string& key, U value) const {
        TypedDataValue<U> typed_value(value);
        Context new_ctx = context_->insert(key, static_cast<DataValue>(typed_value));
        return TypedContext<T>(new_ctx);
    }

    // Typed get with compile-time type safety
    template<typename U>
    [[nodiscard]] std::optional<U> get_typed(const std::string& key) const {
        auto value = context_->get(key);
        if (!value) return std::nullopt;

        // Type-safe extraction based on template parameter
        if constexpr (std::is_same_v<U, int>) {
            if (std::holds_alternative<int>(*value)) {
                return std::get<int>(*value);
            }
        } else if constexpr (std::is_same_v<U, double>) {
            if (std::holds_alternative<double>(*value)) {
                return std::get<double>(*value);
            }
        } else if constexpr (std::is_same_v<U, bool>) {
            if (std::holds_alternative<bool>(*value)) {
                return std::get<bool>(*value);
            }
        } else if constexpr (std::is_same_v<U, std::string>) {
            if (std::holds_alternative<std::string>(*value)) {
                return std::get<std::string>(*value);
            }
        } else if constexpr (std::is_same_v<U, std::vector<std::string>>) {
            if (std::holds_alternative<std::vector<std::string>>(*value)) {
                return std::get<std::vector<std::string>>(*value);
            }
        }

        return std::nullopt; // Type mismatch
    }

    // ===== TYPE EVOLUTION =====
    // Clean transformation between related types without casting

    // insert_as() - Type evolution method
    template<typename U>
    [[nodiscard]] TypedContext<U> insert_as(const std::string& key, auto value) const {
        TypedDataValue<decltype(value)> typed_value(value);
        Context new_ctx = context_->insert(key, static_cast<DataValue>(typed_value));
        return TypedContext<U>(new_ctx);
    }

    // ===== BACKWARD COMPATIBILITY =====
    // Access to underlying Context for runtime flexibility

    // Get underlying context (read-only)
    [[nodiscard]] const Context& base_context() const {
        return *context_;
    }

    // Convert to base Context
    [[nodiscard]] Context to_context() const {
        return *context_;
    }

    // Runtime get (untyped)
    [[nodiscard]] std::optional<DataValue> get(const std::string& key) const {
        return context_->get(key);
    }

    // Check if key exists
    [[nodiscard]] bool has(const std::string& key) const {
        return context_->has(key);
    }

    // Get all keys
    [[nodiscard]] std::vector<std::string> keys() const {
        return context_->keys();
    }

    // Size and empty checks
    [[nodiscard]] size_t size() const {
        return context_->size();
    }

    [[nodiscard]] bool empty() const {
        return context_->empty();
    }

private:
    std::shared_ptr<Context> context_;
};

// ===== TYPE ALIASES =====
// Common typed context patterns

// Empty/any type context (equivalent to untyped)
using ContextAny = TypedContext<std::monostate>;

// String-based context
using ContextString = TypedContext<std::string>;

// Numeric context
using ContextInt = TypedContext<int>;
using ContextDouble = TypedContext<double>;

// Boolean context
using ContextBool = TypedContext<bool>;

// ===== LINK INTERFACE =====
// Generic Link interface for type-safe data transformation

template<typename Input, typename Output>
class Link {
public:
    virtual ~Link() = default;

    // Type-safe call method
    virtual Output call(const Input& input) = 0;

    // Runtime call (for compatibility with untyped chains)
    virtual DataValue call_runtime(const DataValue& input) {
        (void)input; // Suppress unused parameter warning
        // Default implementation - override for custom runtime behavior
        return DataValue(); // Return empty value
    }
};

// ===== CONVENIENCE FUNCTIONS =====

// Create typed context from base context
template<typename T>
TypedContext<T> make_typed_context(const Context& ctx) {
    return TypedContext<T>(ctx);
}

// Create typed context with initial data
template<typename T>
TypedContext<T> make_typed_context(std::unordered_map<std::string, DataValue> data) {
    return TypedContext<T>(std::move(data));
}

// Type-safe context operations
template<typename T, typename U>
TypedContext<T> insert_typed(const TypedContext<T>& ctx, const std::string& key, U value) {
    return ctx.template insert<U>(key, value);
}

} // namespace codeuchain
