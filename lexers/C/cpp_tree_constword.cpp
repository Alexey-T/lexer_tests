    // This seems right in the class tree
    StatusCode Template::findTemplate(const string &id)
    {  return Success; }

    // This does not appear in the class tree
    Id Template::getTemplate(const string &id) const
    { return id;}
