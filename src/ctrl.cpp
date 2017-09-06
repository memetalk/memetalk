#include <QEventLoop>
#include "ctrl.hpp"

ProcessControl::ProcessControl() {
  _running = true;
  _ev = NULL;
}

void ProcessControl::pause() {
  if (_running) {
    if (!_ev) {
      _ev = new QEventLoop;
    }
    _running = false;
    _ev->exec();
  }
}

void ProcessControl::resume() {
  if (!_running) {
    if (!_ev) {
      _ev = new QEventLoop;
    }
    _running = true;
    _ev->exit();
  }
}
