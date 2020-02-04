function App() {
  return (
    <JssProvider jss={jss} generateClassName={generateClassName}>
      ...
    </JssProvider>
  );
  return <div className={this.props.classes.root} />;
}

class MyComponent extends React.Component {
  render () {
    return <div className={this.props.classes.root} />;
  }
}
