import './App.css';
import { useEffect, useState, useCallback } from 'react';
import AstVisualizer from './components/AstVisualizer';

export default () => {
  const [code, setCode] = useState('');
  const [showEditor, setShowEditor] = useState(true);
  const [astData, setAstData] = useState(null);

  const fetchAst = useCallback(async (codeToParse) => {
    localStorage.setItem('savedCode', codeToParse);
    if (!codeToParse.trim()) {
      setAstData(null);
      return;
    }

    const astResponse = await fetch('/ast', {
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain',
      },
      body: codeToParse,
    });

    setAstData(!astResponse.ok
      ? {
        "tag": "Invalid code",
        "contents": "Please try again!",
      }
      : await astResponse.json()
    );
  }, []);

  useEffect(() => {
    const savedCode = localStorage.getItem('savedCode');
    if (savedCode) {
      setCode(savedCode);
      fetchAst(savedCode);
    }
  }, []);

  useEffect(() => {
    const handleKeyDown = (e) => {
      if (e.ctrlKey && e.key === 'Enter') {
        fetchAst(code);
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [code]);

  const handleCodeChange = (e) => {
    const newCode = e.target.value;
    setCode(newCode);
  };

  const toggleEditor = () => setShowEditor(!showEditor);

  return (
    <div className="editor-container">
      <div className="sidebar-toggle">
        <button
          onClick={toggleEditor}
          className="toggle-button"
          aria-label={showEditor ? 'Close Editor' : 'Open Editor'}
        >
          {showEditor ? '«' : '»'}
        </button>
      </div>

      <div className="editor-panel" style={{ width: showEditor ? '300px' : '0' }}>
        {showEditor && (
          <>
            <div className="editor-header">
              <div className="shortcut-hint">
                <span className="shortcut-key">Ctrl+Enter</span>
                <span className="shortcut-text">to generate AST</span>
              </div>
            </div>
            <textarea
              value={code}
              onChange={handleCodeChange}
              placeholder="Type or paste your code here..."
              className="code-editor"
            />
          </>
        )}
      </div>

      <div className="visualizer-container">
        <AstVisualizer astData={astData} />
      </div>
    </div>
  );
};
