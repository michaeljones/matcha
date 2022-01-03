# Gleam Templates

Generate type-safe Gleam modules from text-based template files.

This project provides a Rust program that parses a basic template format and outputs Gleam modules
with 'render' functions that can be imported and called to render the template with different
parameters.

## Syntax

### Value

```
{{ name }}
```

### If

```
{% if is_admin %}Admin{% else %}User{% endif %}
```

The `else` is optional.

### For

```
<ul>
{% for entry in list %}<li>{{ entry }}</li>{% endfor %}
</ul>
```

### Import

```
{> import my_user.{MyUser}
```

### With

```
{> with user_record as MyUser
```

