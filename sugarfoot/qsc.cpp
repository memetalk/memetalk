#include "qsc.hpp"
#include <qscicommandset.h>
#include <QEvent>
#include <QKeyEvent>

#include <iostream>

static QFont getDefaultFont() {
#if defined(Q_OS_WIN32) || defined(Q_OS_WIN64)
  QFont font("Lucida Console", 9);
#elif defined(Q_OS_DARWIN)
  QFont font("Monaco", 14);
#else
  QFont font("Monospace", 9);
#endif
  return font;
}

QColor QsciLexerMemeScript::defaultColor (int style) const {
  if (style == QsciLexerJavaScript::Keyword)
    return QColor("#514CA6");
  if (style == QsciLexerJavaScript::Comment || style == QsciLexerJavaScript::CommentLine)
    return QColor("#29A349");
  if (style == QsciLexerJavaScript::DoubleQuotedString)
    return QColor("#DB0909");
  if (style == QsciLexerJavaScript::Number)
    return QColor("#961212");
  return QColor("black");
}

QFont QsciLexerMemeScript::defaultFont (int style) const {
  QFont font = getDefaultFont();
  if (style == QsciLexerJavaScript::Keyword ||
      style == QsciLexerJavaScript::InactiveKeyword ||
      style == QsciLexerJavaScript::Operator ||
      style == QsciLexerJavaScript::InactiveOperator) {
    font.setBold(true);
  }
  return font;
}

const char * QsciLexerMemeScript::keywords (int set) const {
  if (set != 1) return "";
  return "fun var null this throw return if else while try catch or and super new true false thisModule thisContext";
}

_QScintilla::_QScintilla(QWidget* parent)
  : QsciScintilla(parent) {
  _bg_margin_saved =  QColor("#C5CBE3");
  _bg_margin_changed = QColor("#FF9933");

  CURRENT_LINE_MARKER = 8;
  CURRENT_RANGE_IND = indicatorDefine(DotBoxIndicator);
  _indicators[CURRENT_RANGE_IND] = Position();

// Clearing up some default commands
//as we will rebind them
  QsciCommand* ctrl_d = standardCommands()->find(QsciCommand::SelectionDuplicate);
  ctrl_d->setKey(0);
  // ctrl_l = self.standardCommands().find(QsciCommand.LineCut)
  // ctrl_l.setKey(0)
  // ctrl_x = self.standardCommands().find(QsciCommand.SelectionCut)
  // ctrl_x.setKey(0)
  // ctrl_c = self.standardCommands().find(QsciCommand.SelectionCopy)
  // ctrl_c.setKey(0)
  // ctrl_y = self.standardCommands().find(QsciCommand.Redo)
  // ctrl_y.setKey(0)
  // ctrl_u = self.standardCommands().find(QsciCommand.SelectionLowerCase)
  // ctrl_u.setKey(0)
  // ctrl__ = self.standardCommands().find(QsciCommand.ZoomOut)
  // ctrl__.setKey(0)

  setIndentationsUseTabs(false);
  setIndentationWidth(2);

  QFont font = getDefaultFont();
  setFont(font);
  setMarginsFont(font);

  // Margin 0 is used for line numbers
  QFontMetrics fontmetrics = QFontMetrics(font);
  setMarginsFont(font);
  setMarginWidth(0, fontmetrics.width("000") + 6);
  setMarginLineNumbers(0, true);
  setMarginsBackgroundColor(_bg_margin_saved);

  markerDefine(QsciScintilla::RightArrow, CURRENT_LINE_MARKER);
  setMarkerBackgroundColor(QColor("blue"), CURRENT_LINE_MARKER);

  //Brace matching: enable for a brace immediately before or after
  //the current position
  setBraceMatching(QsciScintilla::SloppyBraceMatch);

  //Current line visible with special background color
  setCaretLineVisible(true);
  setCaretLineBackgroundColor(QColor("#F0F1F7"));

  QsciLexerMemeScript* lexer = new QsciLexerMemeScript();
  lexer->setDefaultFont(font);
  setLexer(lexer);

  //Don't want to see the horizontal scrollbar at all
  //Use raw message to Scintilla here (all messages are documented
  //here: http://www.scintilla.org/ScintillaDoc.html)
  SendScintilla(QsciScintilla::SCI_SETHSCROLLBAR, 0);

  // installEventFilter(this);

  connect(this, SIGNAL(textChanged()), this, SLOT(slot_text_changed()));
}

void _QScintilla::paused_at_line(int start_line, int start_col, int end_line, int end_col) {
  std::cerr << "pause_at_line " << start_line << " " << start_col << " " <<
    end_line << " " << end_col << std::endl;

  // margin arrow
  markerDeleteAll(CURRENT_LINE_MARKER);
  markerAdd(start_line, CURRENT_LINE_MARKER);
  // highlight expression
  if (_indicators.find(CURRENT_RANGE_IND) != _indicators.end() &&
      _indicators[CURRENT_RANGE_IND].is_valid()) {
    Position pos = _indicators[CURRENT_RANGE_IND];
    clearIndicatorRange(pos.start_line,
                        pos.start_col,
                        pos.end_line,
                        pos.end_col,
                        CURRENT_RANGE_IND);
  }
  fillIndicatorRange(start_line, start_col, end_line, end_col, CURRENT_RANGE_IND);
  _indicators[CURRENT_RANGE_IND] =
    Position(start_line, start_col, end_line, end_col);
  ensureLineVisible(start_line);
}

void _QScintilla::setText(const QString& text) {
  QsciScintilla::setText(text);
  setMarginsBackgroundColor(_bg_margin_saved);
}

void _QScintilla::saved() {
  std::cerr << "::save\n";
  setMarginsBackgroundColor(_bg_margin_saved);
}

void _QScintilla::copy() {
  std::cerr << "::copy\n";
  // emacs like behavior: clear the selection
  QsciScintilla::copy();

  int line, index;
  getCursorPosition(&line, &index);
  setCursorPosition(line, index);
}

bool _QScintilla::eventFilter(QObject *obj, QEvent *ev) {
  // // keeps the QMenuBar from stealing the focus if the user
  // // presses ALT
  // if (obj == this && ev->type() == QEvent::ShortcutOverride &&
  //   ((QKeyEvent*)ev)->key() == Qt::Key_Alt &&
  //   ((QKeyEvent*)ev)->modifiers() == Qt::AltModifier)
  //   ev->accept();
  return QsciScintilla::eventFilter(obj, ev);
}

void _QScintilla::slot_text_changed() {
  setMarginsBackgroundColor(_bg_margin_changed);
}

#include "qsc.moc.cpp"
