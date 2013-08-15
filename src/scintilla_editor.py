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

from PyQt4 import QtGui
from PyQt4.Qsci import QsciScintilla, QsciLexerJavaScript, QsciLexerPython, QsciCommand
from PyQt4.QtCore import *
from PyQt4.QtGui import *

class MemeQsciScintilla(QsciScintilla):

    def __init__(self, meme_instance, parent=None):
        super(MemeQsciScintilla, self).__init__(parent)
        self.meme_instance = meme_instance

        self.CURRENT_LINE_MARKER = 8
        self.CURRENT_RANGE_IND = self.indicatorDefine(self.INDIC_DOTBOX)

        self.indicators = {self.CURRENT_RANGE_IND: None}

        # we need to clear up ctrl+d command
        # so QsciScintilla doesn't ignore our QAction
        ctrl_d = self.standardCommands().find(QsciCommand.SelectionDuplicate)
        ctrl_d.setKey(0)

        # Set the default font
        font = QFont()
        #font.setFamily('Courier')
        #font.setFixedPitch(True)
        #font.setPointSize(10)
        self.setFont(font)
        self.setMarginsFont(font)

        # Margin 0 is used for line numbers
        fontmetrics = QFontMetrics(font)
        self.setMarginsFont(font)
        self.setMarginWidth(0, fontmetrics.width("000") + 6)
        self.setMarginLineNumbers(0, True)
        self.setMarginsBackgroundColor(QColor("#cccccc"))

        # Clickable margin 1 for showing markers
        self.setMarginSensitivity(1, True)
        self.setMarginSensitivity(2, True)
        # self.connect(self,
        #     SIGNAL('marginClicked(int, int, Qt::KeyboardModifiers)'),
        #     self.on_margin_clicked)
        # self.markerDefine(QsciScintilla.RightTriangle,
        #     self.ARROW_MARKER_NUM)
        # self.setMarkerBackgroundColor(QColor("#ee1111"),
        #     self.ARROW_MARKER_NUM)

        self.markerDefine(QsciScintilla.RightArrow,self.CURRENT_LINE_MARKER)
        self.setMarkerBackgroundColor(QColor("blue"),self.CURRENT_LINE_MARKER)

        # Brace matching: enable for a brace immediately before or after
        # the current position
        #
        self.setBraceMatching(QsciScintilla.SloppyBraceMatch)

        # Current line visible with special background color
        self.setCaretLineVisible(True)
        self.setCaretLineBackgroundColor(QColor("#ffe4e4"))

        # Set Python lexer
        # Set style for Python comments (style number 1) to a fixed-width
        # courier.
        #
        lexer = QsciLexerJavaScript()#QsciLexerPython()
        lexer.setDefaultFont(font)
        self.setLexer(lexer)
        self.SendScintilla(QsciScintilla.SCI_STYLESETFONT, 1, 'Courier')

        # Don't want to see the horizontal scrollbar at all
        # Use raw message to Scintilla here (all messages are documented
        # here: http://www.scintilla.org/ScintillaDoc.html)
        self.SendScintilla(QsciScintilla.SCI_SETHSCROLLBAR, 0)

    def paused_at_line(self, start_line, start_col, end_line, end_col):
        # margin arrow
        self.markerDeleteAll(self.CURRENT_LINE_MARKER)
        self.markerAdd(start_line, self.CURRENT_LINE_MARKER)
        # highlight expression
        if self.indicators[self.CURRENT_RANGE_IND]:
            ind = self.indicators[self.CURRENT_RANGE_IND]
            self.clearIndicatorRange(ind['start_line'], ind['start_col'], ind['end_line'], ind['end_col'], self.CURRENT_RANGE_IND)
        self.fillIndicatorRange(start_line,start_col,end_line,end_col,self.CURRENT_RANGE_IND)
        self.indicators[self.CURRENT_RANGE_IND] = \
            {'start_line':start_line,'start_col':start_col,'end_line':end_line, 'end_col':end_col}
        self.ensureLineVisible(start_line)

    def on_margin_clicked(self, nmargin, nline, modifiers):
        print str(nmargin) + ":" + str(nline) + ":" + str(modifiers)

        # Toggle marker for the line the margin was clicked on
        if self.markersAtLine(nline) != 0:
            self.markerDelete(nline, self.ARROW_MARKER_NUM)
        else:
            self.markerAdd(nline, self.ARROW_MARKER_NUM)
