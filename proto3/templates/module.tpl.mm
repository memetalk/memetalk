.license{{module.license}}
.endlicense

.preamble({{module_parameters}})
{% for default in default_parameters %}
  {{default.name}} : memetalk/{{default.value}}/1.0();
{% endfor %}
{% for alias in aliases %}
  [{{alias.list}}] <= {{alias.from}};
{% endfor %}

.code

// -- module functions --

{% for name, function in module["compiled_functions"] | dictsort %}
{{name}}: {{function.text | comment(module.name, None, name)}}

{% endfor %}
// -- module classes --

{% for cname, class in module["compiled_classes"] | dictsort %}
{% if class.super_class_name ==  "Object" %}
class {{cname}}
{% else %}
class {{cname}} < {{class.super_class_name }}
{% endif %}
fields: {{class.fields | join(", ")}};
{% for fname,ctor in class.own_methods | dictsort | if_ctor(True)  %}
init {{fname}}: {{ctor.text | comment(module.name, cname, fname)}}

{% endfor %}
{% for fname,f in class.methods | dictsort %}
instance_method {{fname}}: {{f.text | comment(module.name, cname, fname)}}

{% endfor %}
{% for fname,f in class.own_methods | dictsort | if_ctor(False)  %}
class_method {{fname}}: {{f.text | comment(module.name, cname, fname)}}

{% endfor %}
end //{{module.name}}:{{cname}}

{% endfor %}

.end
