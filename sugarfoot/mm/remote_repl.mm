.preamble(qt, io)
  qt : meme:qt;
  io : meme:io;
.code

class RemoteDebugger
fields: process, current_frame_idx, started, server, socket, vars;
init new: fun(proc) {
  @current_frame_idx = 0;
  @vars = {};
  @process = proc;
  @started = false;
  @server = qt.QTcpServer.create_server();
  @server.connect("newConnection", fun() {
    @socket = @server.nextPendingConnection;
    this.client_connected(@socket);
  });
}

instance_method client_connected: fun(socket) {
  io.print("Repl: received connection");
  this.send_location(socket, this.get_frame(@current_frame_idx));
  socket.connect("readyRead", fun() {
     this.read_message(socket);
  });
}

instance_method send_location: fun(socket, frame) {
  var cf = frame.cp().compiledFunction();
  var loc = cf.source_location_for(frame);
  if (loc) {//primitives have loc == null
    socket.write_line("module: " + cf.fullName() + "@" + loc.toString);
  }
}

instance_method send_locals: fun(socket, frame) {
  var variables = frame.cp.compiledFunction.env_table;
  var locals = "";
  variables.each(fun(idx, varname) {
     var value = frame.get_local_value(idx);
     locals = locals + varname.toString + " : " + value.toString + "\n";
  });
  io.print(locals);
  socket.write_line("locals: " + locals.b64encode);
}

instance_method get_frame: fun(idx) {
  return @process.frames()[idx];
}

instance_method process_paused: fun() { //this is called from the vm
  @current_frame_idx = 0;
  if (@started == false) {
    @started = true;
    @server.listen(4200);
  } else {
    this.send_location(@socket, this.get_frame(@current_frame_idx));
    io.print("process paused: nothing");
  }
}

instance_method compile_module: fun(module_name) {
  <primitive "remote_repl_compile_module">
}

instance_method instantiate_module: fun(module_name) {
  <primitive "remote_repl_instantiate_module">
}

// instance_method load_module: fun(socket, module_name) {
//   if (this.compile_module(module_name)) {
//     try {
//       @current_imod = this.instantiate_module(module_name);
//       socket.write_line("* OK");
//     } catch(e) {
//       socket.write_line("* ERR");
//     }
//   } else {
//     socket.write_line("* ERR");
//   }
// }

instance_method doIt: fun(socket, text) {
  try {
    var ctx = Context.withFrame(text, this.get_frame(@current_frame_idx), @process.mp);
    ctx();
  } catch(ex) {
    io.print(ex.message());
    socket.write_line("* ERR");
  }
}

instance_method printIt: fun(socket, text) {
  try {
    var ctx = Context.withFrame(text, this.get_frame(@current_frame_idx), @process.mp);
    var res = ctx();
    io.print(res.toString);
    socket.write_line("show: " + res.toString.b64encode);
  } catch(ex) {
    socket.write_line("* ERR");
  }
}

instance_method parse_location: fun(location) {
  var splt = location.split(":");
  var mod_part = splt[0];
  var fun_part = splt[1].split("@");
  var fun_name = fun_part[0];
  var fun_line = fun_part[1].toInteger - 1; //emacs is 1-based, we are 0-based
  var module_name = null;
  var class_name = null;
  var cmod = null;
  var cclass = null;
  var top_level_cfun = null;
  var cfun = null;
  if (mod_part.find("/") >= 0) {
    var class_part = mod_part.split("/");
    module_name = class_part[0];
    class_name = class_part[1];
    return {:module_name: module_name,
            :class_name: class_name,
            :function_name: fun_name,
            :line: fun_line};
  } else {
    module_name = mod_part;
    return {:module_name: module_name,
            :class_name: null,
            :function_name: fun_name,
            :line: fun_line};
  }
}

instance_method get_cfun_from_location: fun(loc) {
  var cmod = null;
  var cfun = null;
  if (loc[:class_name]) {
    cmod = get_compiled_module_by_name(loc[:module_name]);
    var cclass = cmod.classes[loc[:class_name].toSymbol];
    return cclass.methods[loc[:function_name].toSymbol]; //todo: own methods
  } else {
    cmod = get_compiled_module_by_name(loc[:module_name]);
    return cmod.functions[loc[:function_name].toSymbol];
  }
}

instance_method run_until: fun(socket, location) {
  // io.print("run until: " + location);
  var loc = this.parse_location(location);
  // io.print("run until loc: " + loc.toString);
  var cfun = this.get_cfun_from_location(loc);
  // io.print("run until: " + cfun.name + " at " + loc[:line].toString);
  @process.runUntil(cfun, loc[:line]);
}

instance_method break_at: fun(socket, location) {
  // io.print("break at: " + location);
  var loc = this.parse_location(location);
  // io.print("break-at loc: " + loc.toString);
  var top_level_cfun = this.get_cfun_from_location(loc);
  // io.print("break-at top_level: " + top_level_cfun.name);
  var cfun = null;
  if (top_level_cfun.line_mappings.values().has(loc[:line])) {
    cfun = top_level_cfun;
    // io.print("break-at is on toplevel");
  } else {
    // io.print("break-at is on closure: " + top_level_cfun.closures.toString);
    cfun = top_level_cfun.closures.detect(fun(closure) {
      io.print(closure.line_mappings);
      closure.line_mappings.values().has(loc[:line])
    });
    // io.print("break-at closure: " + cfun.name);
  }
  // io.print("break at: " + cfun.name + " at " + loc[:line].toString);
  @process.add_breakpoint(cfun, loc[:line]);
}

instance_method toggle_module_break_mode: fun(socket) {
  var res = @process.toggle_module_break_mode();
  io.print("Breaking only on functions/methods entered in current module: " + res.toString);
}

instance_method dispatch: fun(socket, command) {
  // if (command.find("load") == 0) {
  //   var module_name = command.from(5);
  //   this.load_module(socket, module_name);
  //   return null;
  // }
  if (command.find("do-it") == 0) {
    this.doIt(socket, command.from(6).b64decode());
    return null;
  }
  if (command.find("print-it") == 0) {
    this.printIt(socket, command.from(9).b64decode());
    return null;
  }
  if (command.find("step-into") == 0) {
    @process.stepInto();
    return null;
  }
  if (command.find("step-over") == 0) {
    @process.stepOver();
    return null;
  }
  if (command.find("step-line") == 0) {
    @process.stepOverLine();
    return null;
  }
  if (command.find("continue") == 0) {
    @process.resume();
    return null;
  }
  if (command.find("locals") == 0) {
    this.send_locals(socket, this.get_frame(@current_frame_idx));
    return null;
  }
  if (command.find("reload-frame") == 0) {
    @process.reloadFrame();
    this.send_location(socket, this.get_frame(@current_frame_idx));
    return null;
  }
  if (command.find("bt-up") == 0) {
    this.move_back_trace_up();
    this.send_location(socket, this.get_frame(@current_frame_idx));
    return null;
  }
  if (command.find("bt-down") == 0) {
    this.move_back_trace_down();
    this.send_location(socket, this.get_frame(@current_frame_idx));
    return null;
  }
  if (command.find("run-until") == 0) {
    this.run_until(socket, command.from(10).b64decode());
    return null;
  }
  if (command.find("break-at") == 0) {
    this.break_at(socket, command.from(9).b64decode());
    return null;
  }
  if (command.find("clear-breaks") == 0) {
    this.clear_break_points();
    return null;
  }
  if (command.find("toggle-module-step-mode") == 0) {
    this.toggle_module_break_mode(socket);
    return null;
  }
  if (command.find("clear-module-break-mode") == 0) {
    this.clear_module_break_mode(socket);
    return null;
  }
  io.print("Unknown command " + command);
  socket.write_line("* ERR");
}

instance_method move_back_trace_up: fun() {
  if (@current_frame_idx < (@process.frames.size - 1)) {
    @current_frame_idx = @current_frame_idx + 1;
  }
}

instance_method move_back_trace_down: fun() {
  if (@current_frame_idx > 0) {
    @current_frame_idx = @current_frame_idx - 1;
  }
}

instance_method read_message: fun(socket) {
  var command = socket.read_string();
  io.print("received: " + command);
  this.dispatch(socket, command);
}
end

debug: fun(proc) {
  var app = qt.QApplication.new(); //in case target process didn't started it
  return RemoteDebugger.new(proc);
}

main: fun() {
  var app = qt.QApplication.new();
  var repl = RemoteRepl.new(null);
  return app.exec();
}

.endcode
