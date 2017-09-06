#ifndef CONTROL_HPP
#define CONTROL_HPP

class QEventLoop;

class ProcessControl {
public:
  ProcessControl();
  void pause();
  void resume();
private:
  bool _running;
  QEventLoop* _ev;
};

#endif
