"""
CodeUChain: insert_as() Method Demonstration

This example demonstrates the insert_as() method which enables clean type evolution
in typed contexts. The insert_as() method allows you to:

1. Add new fields to a TypedDict context without casting
2. Maintain type safety during context evolution
3. Enable progressive data enrichment in chains
4. Support the "evolution pattern" for typed workflows

Key Benefits:
- Type-safe context evolution
- No casting required
- Compile-time guarantees
- Clean separation of concerns
"""

from typing import TypedDict
from codeuchain.core import Context

# =============================================================================
# TYPED DICTS FOR DEMONSTRATION
# =============================================================================

class UserInput(TypedDict):
    """Initial user data."""
    name: str
    email: str


class UserWithValidation(TypedDict):
    """User data after validation."""
    name: str
    email: str
    is_valid: bool


class UserWithProfile(TypedDict):
    """User data with profile information."""
    name: str
    email: str
    is_valid: bool
    profile_complete: bool
    age: int


class UserWithPreferences(TypedDict):
    """User data with preferences."""
    name: str
    email: str
    is_valid: bool
    profile_complete: bool
    age: int
    theme: str
    notifications: bool


# =============================================================================
# DEMONSTRATION FUNCTIONS
# =============================================================================

def validate_email(email: str) -> bool:
    """Simple email validation."""
    return "@" in email and "." in email


def calculate_age_from_birth_year(birth_year: int) -> int:
    """Calculate age from birth year."""
    return 2024 - birth_year


def get_user_preferences(user_id: str) -> dict:
    """Mock function to get user preferences."""
    # In real code, this would query a database
    return {
        "theme": "dark",
        "notifications": True
    }


# =============================================================================
# EVOLUTION PATTERN DEMONSTRATION
# =============================================================================

def demonstrate_context_evolution():
    """
    Demonstrate how insert_as() enables clean context evolution.

    This shows the "evolution pattern" where each step adds new fields
    to the context while maintaining type safety.
    """

    print("=== CodeUChain: insert_as() Method Demonstration ===\n")

    # Start with initial user data
    initial_data: UserInput = {
        "name": "Alice Johnson",
        "email": "alice@example.com"
    }

    print("1. INITIAL CONTEXT:")
    print(f"   Data: {initial_data}")
    print(f"   Type: UserInput")
    print()

    # Step 1: Validate email and add validation result
    print("2. AFTER EMAIL VALIDATION (UserWithValidation):")

    ctx1 = Context[UserInput](initial_data)
    is_valid = validate_email(ctx1.get("email") or "")

    # insert_as() allows type evolution without casting!
    ctx2 = ctx1.insert_as("is_valid", is_valid)

    print(f"   Data: {ctx2.to_dict()}")
    print(f"   Type: UserWithValidation (evolved from UserInput)")
    print(f"   ✅ Added 'is_valid' field via insert_as()")
    print()

    # Step 2: Add profile information
    print("3. AFTER PROFILE COMPLETION (UserWithProfile):")

    # Simulate getting birth year from somewhere
    birth_year = 1990
    age = calculate_age_from_birth_year(birth_year)
    profile_complete = True

    ctx3 = ctx2.insert_as("profile_complete", profile_complete).insert_as("age", age)

    print(f"   Data: {ctx3.to_dict()}")
    print(f"   Type: UserWithProfile (evolved from UserWithValidation)")
    print(f"   ✅ Added 'profile_complete' and 'age' fields via insert_as()")
    print()

    # Step 3: Add user preferences
    print("4. AFTER PREFERENCES LOADING (UserWithPreferences):")

    user_name = ctx3.get("name") or "Unknown"
    preferences = get_user_preferences(user_name)

    ctx4 = ctx3.insert_as("theme", preferences["theme"]).insert_as("notifications", preferences["notifications"])

    print(f"   Data: {ctx4.to_dict()}")
    print(f"   Type: UserWithPreferences (evolved from UserWithProfile)")
    print(f"   ✅ Added 'theme' and 'notifications' fields via insert_as()")
    print()

    # Demonstrate type safety - this would cause a type error if we tried
    # to access a field that doesn't exist in the current type
    print("5. TYPE SAFETY DEMONSTRATION:")
    print("   • ctx1 can only access: name, email")
    print("   • ctx2 can access: name, email, is_valid")
    print("   • ctx3 can access: name, email, is_valid, profile_complete, age")
    print("   • ctx4 can access: name, email, is_valid, profile_complete, age, theme, notifications")
    print()

    # Show how this enables progressive enrichment
    print("6. PROGRESSIVE ENRICHMENT PATTERN:")
    print("   Each step adds more information while maintaining type safety")
    print("   No casting required - insert_as() handles type evolution automatically")
    print("   IDE provides full IntelliSense at each step")
    print()


def demonstrate_error_handling():
    """
    Demonstrate error handling with insert_as().
    """

    print("=== ERROR HANDLING WITH insert_as() ===\n")

    # Start with potentially invalid data
    invalid_data: UserInput = {
        "name": "Bob Smith",
        "email": "invalid-email"  # Invalid email
    }

    print("1. HANDLING VALIDATION ERRORS:")

    ctx = Context[UserInput](invalid_data)
    is_valid = validate_email(ctx.get("email") or "")

    if not is_valid:
        # Can add error information alongside regular data
        ctx = ctx.insert_as("is_valid", False).insert_as("validation_error", "Invalid email format")
    else:
        ctx = ctx.insert_as("is_valid", True)

    result = ctx.to_dict()
    print(f"   Result: {result}")

    if "validation_error" in result:
        print(f"   ⚠️  Validation Error: {result['validation_error']}")
    else:
        print("   ✅ Email is valid")

    print()


def demonstrate_method_chaining():
    """
    Demonstrate method chaining with insert_as().
    """

    print("=== METHOD CHAINING WITH insert_as() ===\n")

    print("1. FLUENT API PATTERN:")

    # Start with minimal data
    initial: UserInput = {
        "name": "Charlie Brown",
        "email": "charlie@example.com"
    }

    # Chain multiple insert_as() calls for fluent API
    result_ctx = (
        Context[UserInput](initial)
        .insert_as("is_valid", True)
        .insert_as("profile_complete", True)
        .insert_as("age", 25)
        .insert_as("theme", "light")
        .insert_as("notifications", False)
    )

    print(f"   Fluent chaining result: {result_ctx.to_dict()}")
    print("   ✅ Multiple fields added in a single expression")
    print()


# =============================================================================
# COMPARISON WITH TRADITIONAL APPROACHES
# =============================================================================

def demonstrate_traditional_vs_insert_as():
    """
    Compare traditional approaches with insert_as().
    """

    print("=== TRADITIONAL VS insert_as() APPROACH ===\n")

    initial_data: UserInput = {
        "name": "Diana Prince",
        "email": "diana@example.com"
    }

    print("1. TRADITIONAL APPROACH (without insert_as()):")

    # Traditional approach requires casting or creating new contexts
    ctx = Context[UserInput](initial_data)

    # This would require casting to add new fields
    # ctx_with_validation = Context[UserWithValidation]({**ctx.to_dict(), "is_valid": True})

    print("   • Requires casting: Context[NewType]({**old_dict, new_field: value})")
    print("   • Error-prone and verbose")
    print("   • No type safety during transition")
    print()

    print("2. insert_as() APPROACH:")

    # Clean evolution with insert_as()
    evolved_ctx = ctx.insert_as("is_valid", True)

    print("   • Clean: ctx.insert_as('field', value)")
    print("   • Type-safe evolution")
    print("   • No casting required")
    print("   • IDE support throughout")
    print()


# =============================================================================
# MAIN DEMONSTRATION
# =============================================================================

def main():
    """Run all demonstrations."""

    print("🎯 CodeUChain insert_as() Method Demo")
    print("=" * 50)
    print()

    demonstrate_context_evolution()
    demonstrate_error_handling()
    demonstrate_method_chaining()
    demonstrate_traditional_vs_insert_as()

    print("=== SUMMARY ===")
    print()
    print("The insert_as() method enables:")
    print("✅ Clean type evolution without casting")
    print("✅ Progressive data enrichment")
    print("✅ Full type safety at compile time")
    print("✅ Fluent API for method chaining")
    print("✅ Error handling with additional context")
    print("✅ IDE IntelliSense support throughout")
    print()
    print("This is the foundation for typed workflows in CodeUChain!")


if __name__ == "__main__":
    main()
