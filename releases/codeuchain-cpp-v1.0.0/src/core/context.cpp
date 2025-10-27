#include "codeuchain/context.hpp"
#include <algorithm>

namespace codeuchain {

Context::Context()
    : data_(std::make_shared<std::unordered_map<std::string, DataValue>>()) {}

Context::Context(std::unordered_map<std::string, DataValue> data)
    : data_(std::make_shared<std::unordered_map<std::string, DataValue>>(std::move(data))) {}

Context::Context(const Context& other)
    : data_(other.data_) {}

Context::Context(Context&& other) noexcept
    : data_(std::move(other.data_)) {}

Context& Context::operator=(const Context& other) {
    if (this != &other) {
        data_ = other.data_;
    }
    return *this;
}

Context& Context::operator=(Context&& other) noexcept {
    if (this != &other) {
        data_ = std::move(other.data_);
    }
    return *this;
}

Context Context::insert(std::string key, DataValue value) const {
    auto new_data = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    new_data->insert_or_assign(std::move(key), std::move(value));
    return Context(std::move(*new_data));
}

std::optional<DataValue> Context::get(const std::string& key) const {
    auto it = data_->find(key);
    if (it != data_->end()) {
        return it->second;
    }
    return std::nullopt;
}

Context Context::update(std::string key, DataValue value) const {
    auto new_data = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    new_data->insert_or_assign(std::move(key), std::move(value));
    return Context(std::move(*new_data));
}

bool Context::has(const std::string& key) const {
    return data_->find(key) != data_->end();
}

std::vector<std::string> Context::keys() const {
    std::vector<std::string> result;
    result.reserve(data_->size());
    for (const auto& [key, _] : *data_) {
        result.push_back(key);
    }
    return result;
}

Context Context::remove(const std::string& key) const {
    auto new_data = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    new_data->erase(key);
    return Context(std::move(*new_data));
}

Context Context::clear() const {
    return Context();
}

size_t Context::size() const {
    return data_->size();
}

bool Context::empty() const {
    return data_->empty();
}

// ===== PERFORMANCE OPTIMIZATION METHODS =====
// For high-frequency mutations within a single link

void Context::insert_mut(std::string key, DataValue value) {
    // Ensure we have exclusive ownership before mutation
    if (data_.use_count() > 1) {
        data_ = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    }
    data_->insert_or_assign(std::move(key), std::move(value));
}

void Context::update_mut(std::string key, DataValue value) {
    // Ensure we have exclusive ownership before mutation
    if (data_.use_count() > 1) {
        data_ = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    }
    data_->insert_or_assign(std::move(key), std::move(value));
}

void Context::remove_mut(const std::string& key) {
    // Ensure we have exclusive ownership before mutation
    if (data_.use_count() > 1) {
        data_ = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    }
    data_->erase(key);
}

void Context::clear_mut() {
    // Ensure we have exclusive ownership before mutation
    if (data_.use_count() > 1) {
        data_ = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    }
    data_->clear();
}

} // namespace codeuchain