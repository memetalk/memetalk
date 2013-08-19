.license{{module.license}}
.endlicense

module {{module.name}}({{module_parameters}})
{% for default in default_parameters %}
  {{default.name}} : memetalk/{{default.value}}/1.0();
{% endfor %}
{% for alias in aliases %}
  [{{alias.list}}] <= {{alias.from}};
{% endfor %}
{
{% for name, function in module["compiled_functions"] | dictsort %}
  {{function.name}}: {{function.text}}

{% endfor %}
{% for name, class in module["compiled_classes"] | dictsort %}
  {% if class.super_class_name ==  "Object" %}
class {{class.name}} {
  {% else %}
class {{class.name}} < {{class.super_class_name }} {
  {% endif %}
  fields: {{class.fields | join(", ")}};
{% for fname,ctor in class.own_methods | dictsort | if_ctor(True)  %}
    init {{ctor.name}}: {{ctor.text}}
{% endfor %}
{% for fname,f in class.methods | dictsort %}
    instance_method {{fname}}: {{f.text}}
{% endfor %}
{% for fname,f in class.own_methods | dictsort | if_ctor(False)  %}
    class_method {{fname}}: {{f.text}}
{% endfor %}
  }
{% endfor %}
}
