#include "codeuchain/state.hpp"
#include <algorithm>

namespace codeuchain {

State::State()
    : data_(std::make_shared<std::unordered_map<std::string, DataValue>>()) {}

State::State(std::unordered_map<std::string, DataValue> data)
    : data_(std::make_shared<std::unordered_map<std::string, DataValue>>(std::move(data))) {}

State::State(const State& other)
    : data_(other.data_) {}

State::State(State&& other) noexcept
    : data_(std::move(other.data_)) {}

State& State::operator=(const State& other) {
    if (this != &other) {
        data_ = other.data_;
    }
    return *this;
}

State& State::operator=(State&& other) noexcept {
    if (this != &other) {
        data_ = std::move(other.data_);
    }
    return *this;
}

State State::insert(std::string key, DataValue value) const {
    auto new_data = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    new_data->insert_or_assign(std::move(key), std::move(value));
    return State(std::move(*new_data));
}

std::optional<DataValue> State::get(const std::string& key) const {
    auto it = data_->find(key);
    if (it != data_->end()) {
        return it->second;
    }
    return std::nullopt;
}

State State::update(std::string key, DataValue value) const {
    auto new_data = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    new_data->insert_or_assign(std::move(key), std::move(value));
    return State(std::move(*new_data));
}

bool State::has(const std::string& key) const {
    return data_->find(key) != data_->end();
}

std::vector<std::string> State::keys() const {
    std::vector<std::string> result;
    result.reserve(data_->size());
    for (const auto& [key, _] : *data_) {
        result.push_back(key);
    }
    return result;
}

State State::remove(const std::string& key) const {
    auto new_data = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    new_data->erase(key);
    return State(std::move(*new_data));
}

State State::clear() const {
    return State();
}

size_t State::size() const {
    return data_->size();
}

bool State::empty() const {
    return data_->empty();
}

// ===== PERFORMANCE OPTIMIZATION METHODS =====
// For high-frequency mutations within a single link

void State::insert_mut(std::string key, DataValue value) {
    // Ensure we have exclusive ownership before mutation
    if (data_.use_count() > 1) {
        data_ = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    }
    data_->insert_or_assign(std::move(key), std::move(value));
}

void State::update_mut(std::string key, DataValue value) {
    // Ensure we have exclusive ownership before mutation
    if (data_.use_count() > 1) {
        data_ = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    }
    data_->insert_or_assign(std::move(key), std::move(value));
}

void State::remove_mut(const std::string& key) {
    // Ensure we have exclusive ownership before mutation
    if (data_.use_count() > 1) {
        data_ = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    }
    data_->erase(key);
}

void State::clear_mut() {
    // Ensure we have exclusive ownership before mutation
    if (data_.use_count() > 1) {
        data_ = std::make_shared<std::unordered_map<std::string, DataValue>>(*data_);
    }
    data_->clear();
}

} // namespace codeuchain