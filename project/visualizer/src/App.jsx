import './App.css';
import { useEffect, useState } from 'react';
import AstVisualizer from './components/AstVisualizer';

function App() {
  const [astData, setAstData] = useState(null);

  useEffect(() => {
    const fetchAst = async () => {
      try {
        const response = await fetch('/ast.json');
        setAstData(await response.json());
      } catch (error) {
        console.error('Error loading AST:', error);
      }
    };

    fetchAst();
  }, []);

  return (
    <div className="App">
      <main>
        {astData ? <AstVisualizer astData={astData} /> : <p>Loading AST...</p>}
      </main>
    </div>
  );
}

export default App;
