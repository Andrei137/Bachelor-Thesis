import * as d3 from 'd3';
import { useEffect, useRef } from 'react';

export default ({ astData }) => {
  if (astData === null) {
    return;
  }

  const svgRef = useRef(null);
  const gRef = useRef(null);
  const zoomRef = useRef(null);
  const colors = {
    'Expr'        : '#4ECDC4',
    'Read'        : '#FFD166',
    'Print'       : '#06D6A0',
    'Declare'     : '#22C3E8',
    'Assign'      : '#EF476F',
    'If'          : '#06D6A0',
    'FuncDef'     : '#FFD166',
    'Return'      : '#A2D2FF',
    'Seq'         : '#8AC926',
    'Invalid code': '#DA0000',
  };
  const searchRegex = (tag) => {
    const toColor = (pattern, color) => ({ pattern, color });
    const regexColors = [
      toColor(/.*Const$/, '#BDE0FE'),
      toColor(/(Break|Continue)/, '#FF6B6B'),
      toColor(/(While|For)/, '#CCCCCC')
    ];
    for (const { pattern, color } of regexColors) {
      if (pattern.test(tag)) return color;
    }
    return null;
  };
  const getNodeColor = (tag) => colors[tag] || searchRegex(tag) || '#97C2FC';

  const fitToBox = () => {
    if (!svgRef.current || !gRef.current) return;

    const svg = d3.select(svgRef.current);
    const innerWidth = svgRef.current.clientWidth;
    const innerHeight = svgRef.current.clientHeight;

    const nodes = d3.select(gRef.current).selectAll('.node').data();
    if (!nodes || nodes.length === 0) return;

    const xExtent = d3.extent(nodes, (d) => d.x);
    const yExtent = d3.extent(nodes, (d) => d.y);
    const treeWidth = xExtent[1] - xExtent[0];
    const treeHeight = yExtent[1] - yExtent[0];

    const scale = Math.min(
      innerWidth / Math.max(treeWidth, 1),
      innerHeight / Math.max(treeHeight, 1)
    ) * 0.7;

    const translateX = (innerWidth - treeWidth * scale) / 2 - xExtent[0] * scale;
    const translateY = (innerHeight - treeHeight * scale) / 2 - yExtent[0] * scale;

    if (zoomRef.current) {
      svg
        .transition()
        .duration(500)
        .call(zoomRef.current.transform, d3.zoomIdentity
          .translate(translateX, translateY)
          .scale(scale)
        );
    }
  };

  useEffect(() => {
    if (!astData || !svgRef.current) return;
    d3.select(svgRef.current).selectAll('*').remove();

    const innerWidth = svgRef.current.clientWidth;
    const innerHeight = svgRef.current.clientHeight;

    const processNode = (node) => {
      if (node === null) return {
        label: 'null',
      };
      if (Array.isArray(node)) {
        return {
          label: '[..]',
          children: node.map(processNode),
        };
      }
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
      .style('border-radius', '8px')
      .style('user-select', 'text');

    const group = svg
      .append('g')
      .attr('class', 'zoomable-group');

    group
      .append('g')
      .attr('class', 'text-measure')
      .style('opacity', 0)
      .selectAll('text')
      .data(nodes)
      .enter()
      .append('text')
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
      .style('cursor', 'text')
      .on('mousedown', (event) => {
        event.stopPropagation();
      })
      .on('touchstart', (event) => {
        event.stopPropagation();
      })
      .each(function(_) {
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
          .style('user-select', 'text')
          .text((d) => d.data.label);
      });

    gRef.current = group.node();

    zoomRef.current = d3
      .zoom()
      .scaleExtent([0.1, 8])
      .filter((event) => {
        return !event.target.closest || !event.target.closest('.node');
      })
      .on('zoom', (event) => {
        group.attr('transform', event.transform);
      });

    svg.call(zoomRef.current);
    fitToBox();

  }, [astData]);

  useEffect(() => {
    const resizeObserver = new ResizeObserver(() => {
      fitToBox();
    });

    if (svgRef.current) {
      resizeObserver.observe(svgRef.current);
    }

    return () => {
      resizeObserver.disconnect();
    };
  }, []);

  return (
    <div style={{ width: '100%', height: '100vh' }}>
      <svg ref={svgRef}></svg>
    </div>
  );
};