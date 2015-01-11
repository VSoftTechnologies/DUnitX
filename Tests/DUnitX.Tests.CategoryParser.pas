unit DUnitX.Tests.CategoryParser;

interface

uses
  DUnitX.TestFrameWork,
  DUnitX.Filters,
  DUnitX.CategoryExpression;

type
  [TestFixture]
  TCategoryParseTests = class
  public
    [Test]
    procedure EmptyStringReturnsEmptyFilter;

    [Test]
    procedure CanParseSimpleCategory;

    [Test]
    procedure CanParseCompoundCategory;

    [Test]
    procedure CanParseExcludedCategories;

    [Test]
    procedure CanParseMultipleCategoriesWithAnd;

    [Test]
    procedure CanParseMultipleAlternatives;

    [Test]
    procedure PrecedenceTest;

    [Test]
    procedure PrecedenceTestWithParentheses;

    [Test]
    procedure OrAndMinusCombined;

    [Test]
    procedure PlusAndMinusCombined;

  end;

implementation

{ TCategoryParseTests }

procedure TCategoryParseTests.CanParseCompoundCategory;
var
  filter : ITestFilter;
  catFilter : ICategoryFilter;
begin
  filter := TCategoryExpression.CreateFilter('One , Two; Three,Four');
  catFilter := filter as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'One');
  Assert.AreEqual(catFilter.Categories[1],'Two');
  Assert.AreEqual(catFilter.Categories[2],'Three');
  Assert.AreEqual(catFilter.Categories[3],'Four');
end;

procedure TCategoryParseTests.CanParseExcludedCategories;
var
  filter : ITestFilter;
  catFilter : ICategoryFilter;
  notFilter : INotFilter;
begin
  filter := TCategoryExpression.CreateFilter('-One,Two,Three');
  notFilter := filter as INotFilter;
  Assert.IsNotNull(notFilter);
  catFilter := notFilter.BaseFilter as ICategoryFilter;
  Assert.IsNotNull(catFilter);
  Assert.AreEqual(catFilter.Categories[0],'One');
  Assert.AreEqual(catFilter.Categories[1],'Two');
  Assert.AreEqual(catFilter.Categories[2],'Three');
end;

procedure TCategoryParseTests.CanParseMultipleAlternatives;
var
  filter : ITestFilter;
  orFilter : IOrFilter;
  catFilter : ICategoryFilter;
begin
  filter := TCategoryExpression.CreateFilter('One|Two|Three');
  orFilter := filter as IOrFilter;
  Assert.IsNotNull(orFilter);
  Assert.AreEqual(orFilter.Filters.Count,3);

  catFilter := orFilter.Filters[0] as ICategoryFilter;
  Assert.IsNotNull(catFilter);
  Assert.AreEqual(catFilter.Categories[0],'One');

  catFilter := orFilter.Filters[1] as ICategoryFilter;
  Assert.IsNotNull(catFilter);
  Assert.AreEqual(catFilter.Categories[0],'Two');

  catFilter := orFilter.Filters[2] as ICategoryFilter;
  Assert.IsNotNull(catFilter);
  Assert.AreEqual(catFilter.Categories[0],'Three');
end;

procedure TCategoryParseTests.CanParseMultipleCategoriesWithAnd;
var
  filter : ITestFilter;
  catFilter : ICategoryFilter;
  andFilter : IAndFilter;
begin
  filter := TCategoryExpression.CreateFilter('One + Two+Three');
  andFilter := filter as IAndFilter;
  Assert.IsNotNull(andFilter);
  Assert.AreEqual(andFilter.Filters.Count,3);
  catFilter := andFilter.Filters[0] as ICategoryFilter;
  Assert.IsNotNull(catFilter);
  Assert.AreEqual(catFilter.Categories[0],'One');

  catFilter := andFilter.Filters[1] as ICategoryFilter;
  Assert.IsNotNull(catFilter);
  Assert.AreEqual(catFilter.Categories[0],'Two');

  catFilter := andFilter.Filters[2] as ICategoryFilter;
  Assert.IsNotNull(catFilter);
  Assert.AreEqual(catFilter.Categories[0],'Three');
end;

procedure TCategoryParseTests.CanParseSimpleCategory;
var
  filter : ITestFilter;
  catFilter : ICategoryFilter;
begin
  filter := TCategoryExpression.CreateFilter('Data');
  catFilter := filter as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'Data');
end;

procedure TCategoryParseTests.EmptyStringReturnsEmptyFilter;
var
  filter : ITestFilter;
begin
  filter := TCategoryExpression.CreateFilter('');
  Assert.IsNotNull(filter);
  Assert.IsTrue(filter.IsEmpty);
end;

procedure TCategoryParseTests.OrAndMinusCombined;
var
  filter : ITestFilter;
  orFilter : IOrFilter;
  andFilter : IAndFilter;
begin
  filter := TCategoryExpression.CreateFilter('A|B-C-D|E');
  orFilter := filter as IOrFilter;
  Assert.IsNotNull(orFilter);
  Assert.AreEqual(orFilter.Filters.Count,3);
  andFilter := orFilter.Filters[1] as IAndFilter;
  Assert.IsNotNull(andFilter);
  Assert.AreEqual(andFilter.Filters.Count,3);
  Assert.Implements<ICategoryFilter>(andFilter.Filters[0]);
  Assert.Implements<INotFilter>(andFilter.Filters[1]);
  Assert.Implements<INotFilter>(andFilter.Filters[2]);
end;

procedure TCategoryParseTests.PlusAndMinusCombined;
var
  filter : ITestFilter;
  andFilter : IAndFilter;
begin
  filter := TCategoryExpression.CreateFilter('A+B-C-D+E');
  andFilter := filter as IAndFilter;
  Assert.IsNotNull(andFilter);
  Assert.AreEqual(andFilter.Filters.Count,5);
  Assert.Implements<ICategoryFilter>(andFilter.Filters[0]);
  Assert.Implements<ICategoryFilter>(andFilter.Filters[1]);
  Assert.Implements<INotFilter>(andFilter.Filters[2]);
  Assert.Implements<INotFilter>(andFilter.Filters[3]);
  Assert.Implements<ICategoryFilter>(andFilter.Filters[4]);
end;

procedure TCategoryParseTests.PrecedenceTest;
var
  filter : ITestFilter;
  orFilter : IOrFilter;
  andFilter : IAndFilter;
  catFilter : ICategoryFilter;
  notFilter : INotFilter;
begin
  filter := TCategoryExpression.CreateFilter('A + B | C + -D,E,F');
  orFilter := filter as IorFilter;
  Assert.IsNotNull(orFilter);

  andFilter := orFilter.Filters[0] as IAndFilter;
  Assert.IsNotNull(andFilter);
  catFilter := andFilter.Filters[0] as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'A');
  catFilter := andFilter.Filters[1] as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'B');

  andFilter := orFilter.Filters[1] as IAndFilter;
  Assert.IsNotNull(andFilter);
  catFilter := andFilter.Filters[0] as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'C');

  notFilter := andFilter.Filters[1] as INotFilter;
  Assert.IsNotNull(notFilter);
  catFilter := notFilter.BaseFilter as ICategoryFilter;
  Assert.IsNotNull(catFilter);
  Assert.AreEqual(catFilter.Categories[0],'D');
  Assert.AreEqual(catFilter.Categories[1],'E');
  Assert.AreEqual(catFilter.Categories[2],'F');
end;

procedure TCategoryParseTests.PrecedenceTestWithParentheses;
var
  filter : ITestFilter;
  orFilter : IOrFilter;
  andFilter : IAndFilter;
  catFilter : ICategoryFilter;
  notFilter : INotFilter;
begin
  filter := TCategoryExpression.CreateFilter('A + (B | C) - D,E,F');
  andFilter := filter as IAndFilter;
  Assert.IsNotNull(andFilter);
  Assert.AreEqual(andFilter.Filters.Count,3);

  catFilter := andFilter.Filters[0] as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'A');

  orFilter := andFilter.Filters[1] as IOrFilter;
  Assert.IsNotNull(orFilter);
  catFilter := orFilter.Filters[0] as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'B');
  catFilter := orFilter.Filters[1] as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'C');

  notFilter := andFilter.Filters[2] as INotFilter;
  Assert.IsNotNull(notFilter);
  catFilter := notFilter.BaseFilter as ICategoryFilter;
  Assert.AreEqual(catFilter.Categories[0],'D');
  Assert.AreEqual(catFilter.Categories[1],'E');
  Assert.AreEqual(catFilter.Categories[2],'F');
end;

initialization
  TDUnitX.RegisterTestFixture(TCategoryParseTests);
end.
