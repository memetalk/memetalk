#ifndef QSC_HPP
#define QSC_HPP
#include <Qsci/qsciscintilla.h>
#include <Qsci/qscicommand.h>
#include <Qsci/qscilexerjavascript.h>
#include <Qsci/qsciscintilla.h>
#include <QColor>
#include <map>

class QsciLexerMemeScript : public QsciLexerJavaScript {
public:
  QColor defaultColor (int style) const;
  QFont defaultFont (int style) const;
  const char * keywords (int set) const;
};

class Position {
public:
  Position(int _start_line, int _start_col, int _end_line, int _end_col)
    : start_line(_start_line),
      end_line(_end_line),
      start_col(_start_col),
      end_col(_end_col) {};

  Position() : start_line(-1), end_line(-1), start_col(-1), end_col(-1) {};
  int start_line;
  int end_line;
  int start_col;
  int end_col;
  bool is_valid() { return start_line != -1; };
};

class _QScintilla : public QsciScintilla {
 Q_OBJECT
public:
  _QScintilla(QWidget* parent);

  void paused_at_line(int start_line, int start_col, int end_line, int end_col);
  virtual void setText(const QString&);
  void saved();
  void copy();
protected:
  bool eventFilter(QObject *obj, QEvent *ev);

private slots:
  void slot_text_changed();

private:
  int CURRENT_LINE_MARKER;
  int CURRENT_RANGE_IND;

  QColor _bg_margin_saved;
  QColor _bg_margin_changed;
  std::map<int, Position> _indicators;

};

#endif
