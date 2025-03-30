import * as d3 from 'd3';
import { useEffect, useRef } from 'react';

const AstVisualizer = ({ astData }) => {
  const svgRef = useRef(null);
  const gRef = useRef(null);
  const colors = {
    'Read'    : '#FFD166',
    'Print'   : '#06D6A0',
    'While'   : '#118AB2',
    'Assign'  : '#EF476F',
    'Var'     : '#A2D2FF',
    'IntConst': '#BDE0FE',
    'Seq'     : '#8AC926',
    'Basic'   : '#FFFFFF',
  };

  const getNodeColor = (tag) => colors[tag] || '#97C2FC';

  useEffect(() => {
    if (!astData || !svgRef.current) return;
    d3.select(svgRef.current).selectAll('*').remove();

    const innerWidth = svgRef.current.clientWidth - 100;
    const innerHeight = svgRef.current.clientHeight;

    const processNode = (node) => {
      if (node === null) return null;
      if (node.tag) {
        return {
          tag: node.tag,
          label: node.tag,
          children: node.contents !== undefined
            ? (Array.isArray(node.contents)
                ? node.contents.map(processNode)
                : [processNode(node.contents)])
            : [],
        };
      }
      return {
        label: node.toString(),
        children: [],
      };
    };

    const offset = 25;
    const root = d3.hierarchy(processNode(astData));
    const treeLayout = d3
      .tree()
      .size([innerWidth, innerHeight])
      .nodeSize([offset * 3, offset * 6]);

    treeLayout(root);
    const nodes = root.descendants();

    const svg = d3
      .select(svgRef.current)
      .attr('width', '100%')
      .attr('height', '100%')
      .style('background-color', '#f8f9fa')
      .style('border', '1px solid #ddd')
      .style('border-radius', '8px');

    const group = svg
      .append('g')
      .attr('class', 'zoomable-group');

    group
      .append('g')
      .attr('class', 'text-measure')
      .style('opacity', 0)
      .selectAll('text')
      .data(nodes)
      .enter().append('text')
      .attr('font-size', '12px')
      .text((d) => d.data.label)
      .each(function(d) {
        const bbox = this.getBBox();
        d.data.width = Math.max(bbox.width, 0) + offset;
        d.data.height = Math.max(bbox.height, 0) + offset;
      })
      .remove();

    // Edges
    group
      .selectAll('.link')
      .data(root.links())
      .enter()
      .append('path')
      .attr('class', 'link')
      .attr('d', (d) => {
        const source = { x: d.source.x, y: d.source.y };
        const target = { x: d.target.x, y: d.target.y };
        const midX = (source.x + target.x) / 2;
        const midY = (source.y + target.y) / 2;
        return `M${source.x},${source.y} Q${midX},${midY} ${target.x},${target.y}`;
      })
      .attr('stroke', '#6c757d')
      .attr('stroke-width', 2)
      .attr('fill', 'none');

    // Nodes
    group
      .selectAll('.node')
      .data(nodes)
      .enter()
      .append('g')
      .attr('class', 'node')
      .attr('transform', (d) => `translate(${d.x}, ${d.y})`)
      .each(function(d) {
        d3.select(this)
          .append('rect')
          .attr('width', (d) => d.data.width)
          .attr('height', (d) => d.data.height)
          .attr('x', (d) => -d.data.width / 2)
          .attr('y', (d) => -d.data.height / 2)
          .attr('rx', 4)
          .attr('ry', 4)
          .attr('fill', (d) => getNodeColor(d.data.tag))
          .attr('stroke', '#333')
          .attr('stroke-width', 1);

        d3.select(this)
          .append('text')
          .attr('dy', '.35em')
          .attr('text-anchor', 'middle')
          .attr('font-size', '12px')
          .attr('fill', '#333')
          .text((d) => d.data.label);
      });

    const xExtent = d3.extent(nodes, (d) => d.x);
    const yExtent = d3.extent(nodes, (d) => d.y);
    const treeWidth = xExtent[1] - xExtent[0];
    const treeHeight = yExtent[1] - yExtent[0];
    const scale = Math.min(innerWidth / treeWidth, innerHeight / treeHeight);
    const translateX = (innerWidth - treeWidth * scale) / 2 - xExtent[0] * scale + offset;
    const translateY = (innerHeight - treeHeight * scale) / 2 - yExtent[0] * scale + offset;

    const zoom = d3
      .zoom()
      .scaleExtent([0.1, 8])
      .on('zoom', (event) => {
        group.attr('transform', event.transform);
      });

    gRef.current = group.node();
    svg
      .call(zoom)
      .call(zoom.transform, d3
        .zoomIdentity
        .translate(translateX, translateY)
        .scale(scale)
      );

  }, [astData]);

  return (
    <div style={{ width: '100%', height: '100vh' }}>
      <svg ref={svgRef}></svg>
    </div>
  );
};

export default AstVisualizer;
