# CodeUChain Library Structure: Modular Organization

## Overview

CodeUChain embraces **extreme modularity** with a clear separation of concerns, enabling AI to maintain core protocols while humans oversee project-specific implementations. This structure draws wisdom from modern application architectures while maintaining the philosophy of flexible design.

## Structure Wisdom

### Core Principle: Separation of Concerns
- **AI maintains**: Base protocols and fundamental contracts
- **Humans oversee**: Project composition and component selection
- **Everyone benefits**: Clear boundaries enable rapid prototyping and easy maintenance

### Architectural Mapping

| Traditional App | CodeUChain | Responsibility |
|----------------|------------|---------------|
| Components | Links | Reusable processing units |
| Pages/Features | Chains | Orchestrated workflows |
| Utils | Hook | Cross-cutting concerns |
| Business Logic | Components | Domain-specific implementations |

## Directory Structure

```
codeuchain/
├── core/           # 🤖 AI Territory - Protocols & Base Classes
│   ├── __init__.py
│   ├── state.py  # State protocol & immutable base
│   ├── link.py     # Link processing protocol
│   ├── chain.py    # Chain orchestration protocol
│   └── hook.py # Hook enhancement protocol
├── utils/          # 🛠️ Shared Territory - Common Utilities
│   ├── __init__.py
│   └── error_handling.py # Error handling mixins & utilities
└── examples/       # 👥 Human Territory - Project Compositions
    ├── __init__.py
    └── math_chain/ # Example: Math processing workflow
        ├── __init__.py
        ├── links.py    # Project-specific link implementations
        ├── chains.py   # Project-specific chain compositions
        └── hook.py # Project-specific hook
```

## Usage Patterns

### 1. Basic Usage (Library Components)
```python
from codeuchain import State, BasicChain, MathLink, LoggingHook

# Use library-provided components
chain = BasicChain()
chain.add_link("sum", MathLink("sum"))
chain.use_hook(LoggingHook())
```

### 2. Custom Components (Project-Specific)
```python
# In your project: examples/my_project/links.py
from codeuchain.core import State, Link

class MyCustomLink(Link):
    async def call(self, ctx: State) -> State:
        # Your custom logic
        return ctx.insert("result", "custom_value")
```

### 3. Project Composition (Human Oversight)
```python
# In your project: examples/my_project/chains.py
from codeuchain.components import BasicChain
from .links import MyCustomLink
from .hook import MyCustomHook

def create_my_workflow():
    chain = BasicChain()
    chain.add_link("custom", MyCustomLink())
    chain.use_hook(MyCustomHook())
    return chain
```

## Benefits of This Structure

### 🤖 AI Benefits
- **Focused maintenance**: Only touch core/ protocols
- **Predictable changes**: Protocol changes are rare and well-defined
- **Rapid prototyping**: Generate new components without touching core

### 👥 Human Benefits
- **Easy swapping**: Replace components without touching core logic
- **Project isolation**: Each project has its own examples/ directory
- **Clear ownership**: Know exactly what to maintain vs. what to reuse

### 🔄 Ecosystem Benefits
- **Extreme modularity**: Mix and match components across projects
- **Version compatibility**: Core protocols rarely change
- **Shared utilities**: Common patterns available to all projects

## Migration Guide

### From Flat Structure to Modular
1. **Move protocols to core/**
   - Base classes and protocols → `core/`
   - Abstract interfaces → `core/`

2. **Move implementations to components/**
   - Concrete classes → `components/`
   - Default implementations → `components/`

3. **Move utilities to utils/**
   - Helper functions → `utils/`
   - Mixins → `utils/`

4. **Create project examples/**
   - Project-specific code → `examples/your_project/`
   - Custom implementations → `examples/your_project/`

## Best Practices

### Core Development (AI Focus)
- Keep protocols minimal and stable
- Use Protocol classes for interfaces
- Avoid concrete implementations in core/

### Component Development (Human Focus)
- Implement protocols from core/
- Make components easily swappable
- Document component contracts clearly

### Project Development (Human Oversight)
- Compose components into workflows
- Create project-specific implementations
- Document project requirements and constraints

## Design Philosophy in Structure

This structure embodies **modular design principles** through:
- **Flexibility**: Core serves all implementations equally
- **Maintainability**: Easy to swap components without breaking existing code
- **Harmony**: Clear boundaries prevent conflicts
- **Extensibility**: Easy to extend without modifying existing code

The result is a system where AI can maintain the foundation with confidence, humans can rapidly prototype and swap implementations with ease, and the entire ecosystem grows harmoniously.