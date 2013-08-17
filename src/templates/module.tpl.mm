.license{{module_license}}
.endlicense

module {{module_name}}({{module_parameters}})
{% for default in default_parameters %}
  {{default.name}} : memetalk/{{default.value}}/1.0();
{% endfor %}
{% for alias in aliases %}
  [{{alias.list}}] <= {{alias.from}};
{% endfor %}
{
{% for function in functions %}
  {{function.name}}: {{function.text}}

{% endfor %}
{%for class in classes%}
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
