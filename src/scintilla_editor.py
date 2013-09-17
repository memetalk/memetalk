# Copyright (c) 2012-2013 Thiago B. L. Silva <thiago@metareload.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

# Based on an example published by Eli Bendersky (eliben@gmail.com)

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from PyQt4.Qsci import QsciScintilla, QsciLexerJavaScript, QsciLexerPython, QsciCommand
from pdb import set_trace as br

class QsciLexerMemeScript(QsciLexerJavaScript):
    def __init__(self):
        super(QsciLexerJavaScript, self).__init__()

    def keywords(self, s):
        if s != 1: return ""
        return "fun var null this throw return if else while try catch or and super new true false thisModule thisContext"

    def defaultFont(self, s):
        font = QFont("Monospace",10)
        if s == QsciLexerJavaScript.Keyword or\
                s == QsciLexerJavaScript.InactiveKeyword or\
                s == QsciLexerJavaScript.Operator or\
                s == QsciLexerJavaScript.InactiveOperator:
            font.setBold(True)
        return font

    def defaultColor(self, s):
        if s == QsciLexerJavaScript.Keyword:
            return QColor('#514CA6')
        elif s == QsciLexerJavaScript.Comment or s == QsciLexerJavaScript.CommentLine:
            return QColor('#29A349')
        elif s == QsciLexerJavaScript.DoubleQuotedString:
            return QColor('#DB0909')
        elif s == QsciLexerJavaScript.Number:
            return QColor('#961212')
        return QColor('black')

class MemeQsciScintilla(QsciScintilla):

    def __init__(self, meme_instance, parent=None):
        super(MemeQsciScintilla, self).__init__(parent)
        self.meme_instance = meme_instance

        self.bg_margin_changed  = QColor("#FF9933")
        self.bg_margin_saved =  QColor("#C5CBE3")

        self.CURRENT_LINE_MARKER = 8
        self.CURRENT_RANGE_IND = str(self.indicatorDefine(self.INDIC_DOTBOX))

        self.indicators = {self.CURRENT_RANGE_IND: None}

        # On idez/Editor;
        # select all (ctrl+x,h)
        # cut (ctrl+w)
        # copy (alt+w)
        # paste (ctrl+k


        # Clearing up some default commands
        # as we will rebind them
        ctrl_d = self.standardCommands().find(QsciCommand.SelectionDuplicate)
        ctrl_d.setKey(0)
        ctrl_l = self.standardCommands().find(QsciCommand.LineCut)
        ctrl_l.setKey(0)
        ctrl_x = self.standardCommands().find(QsciCommand.SelectionCut)
        ctrl_x.setKey(0)
        ctrl_c = self.standardCommands().find(QsciCommand.SelectionCopy)
        ctrl_c.setKey(0)
        ctrl_y = self.standardCommands().find(QsciCommand.Redo)
        ctrl_y.setKey(0)
        ctrl_u = self.standardCommands().find(QsciCommand.SelectionLowerCase)
        ctrl_u.setKey(0)
        ctrl__ = self.standardCommands().find(QsciCommand.ZoomOut)
        ctrl__.setKey(0)

        # ctrl_v = self.standardCommands().find(QsciCommand.Paste)
        # ctrl_v.setKey(0)

        self.setIndentationsUseTabs(False)
        self.setIndentationWidth(2)

        # Set the default font
        font = QFont('Monospace',10)
        self.setFont(font)
        self.setMarginsFont(font)

        # Margin 0 is used for line numbers
        fontmetrics = QFontMetrics(font)
        self.setMarginsFont(font)
        self.setMarginWidth(0, fontmetrics.width("000") + 6)
        self.setMarginLineNumbers(0, True)
        self.setMarginsBackgroundColor(self.bg_margin_saved)

        self.markerDefine(QsciScintilla.RightArrow,int(self.CURRENT_LINE_MARKER))
        self.setMarkerBackgroundColor(QColor("blue"),int(self.CURRENT_LINE_MARKER))

        # Brace matching: enable for a brace immediately before or after
        # the current position
        #
        self.setBraceMatching(QsciScintilla.SloppyBraceMatch)

        # Current line visible with special background color
        self.setCaretLineVisible(True)
        self.setCaretLineBackgroundColor(QColor("#F0F1F7"))

        # Set Python lexer
        # Set style for Python comments (style number 1) to a fixed-width
        # courier.
        #
        lexer = QsciLexerMemeScript()
        lexer.setDefaultFont(font)
        self.setLexer(lexer)
        #self.SendScintilla(QsciScintilla.SCI_STYLESETFONT, 1, 'Monospace')

        # Don't want to see the horizontal scrollbar at all
        # Use raw message to Scintilla here (all messages are documented
        # here: http://www.scintilla.org/ScintillaDoc.html)
        self.SendScintilla(QsciScintilla.SCI_SETHSCROLLBAR, 0)

        self.installEventFilter(self)
        self.textChanged.connect(self.slot_text_changed)

    def paused_at_line(self, start_line, start_col, end_line, end_col):
        # margin arrow
        self.markerDeleteAll(int(self.CURRENT_LINE_MARKER))
        self.markerAdd(start_line, int(self.CURRENT_LINE_MARKER))
        # highlight expression
        if self.indicators[self.CURRENT_RANGE_IND]:
            ind = self.indicators[self.CURRENT_RANGE_IND]
            self.clearIndicatorRange(ind['start_line'], ind['start_col'], ind['end_line'], ind['end_col'], int(self.CURRENT_RANGE_IND))
        self.fillIndicatorRange(start_line,start_col,end_line,end_col,int(self.CURRENT_RANGE_IND))
        self.indicators[self.CURRENT_RANGE_IND] = \
            {'start_line':start_line,'start_col':start_col,'end_line':end_line, 'end_col':end_col}
        self.ensureLineVisible(start_line)

    def eventFilter(self, widget, event):
        # keeps the QMenuBar from stealing the focus if the user
        # presses ALT
        if (widget is self and event.type() == QEvent.ShortcutOverride and\
                event.key() == Qt.Key_Alt and event.modifiers() == Qt.AltModifier):
            event.accept()
        return QsciScintilla.eventFilter(self, widget, event)

    def set_text(self, text):
        self.setText(text)
        self.setMarginsBackgroundColor(self.bg_margin_saved)

    def slot_text_changed(self):
        self.setMarginsBackgroundColor(self.bg_margin_changed)

    def saved(self):
        self.setMarginsBackgroundColor(self.bg_margin_saved)

    def copy(self):
        # emacs like behavior: clear the selection
        super(MemeQsciScintilla, self).copy()
        self.setCursorPosition(*self.getCursorPosition())
