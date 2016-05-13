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
  if (command.find("test") == 0) {
    @process.test();
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
